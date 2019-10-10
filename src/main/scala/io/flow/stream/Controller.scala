package io.flow.stream
import com.github.ghik.silencer.silent
import io.apibuilder.spec.v0.models.{Field, Service, UnionType}
import io.apibuilder.validation.{ApiBuilderService, ApiBuilderType, MultiService}
import io.flow.build.{Application, BuildType, Downloader}
import io.flow.util.{FlowEnvironment, StreamNames, VersionParser}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

case class Controller() extends io.flow.build.Controller {
  private val BannedStreamNames = Set(
    "production.local.item.event.v0.local_item_event.json",
    "production.localized.item.internal.event.v0.localized_item_event.json"
  )

  override val name = "Stream"
  override val command = "stream"


  override def run(buildType: BuildType, downloader: Downloader, services: Seq[Service])(implicit ec: ExecutionContext): Unit = {

    @tailrec
    def loadImports(services: Seq[Service], cached: Map[String, Service]): Seq[Service] = {
      val imports = services.flatMap(_.imports).groupBy(_.uri).values.toList.map(_.last)
      val filteredImports = imports.filterNot(imp => services.exists(svc => svc.organization.key == imp.organization.key && svc.application.key == imp.application.key))
      if (filteredImports.isEmpty) {
        services
      } else {
        val importedServices = filteredImports.flatMap { imp =>
          cached.get(imp.organization.key + imp.application.key).orElse {
            println("Loading imports")
            downloader.service(Application(imp.organization.key, imp.application.key, Application.Latest)) match {
              case Right(svc) => Some(svc)
              case Left(_) =>
                println(s"[ERROR] Failed to fetch import ${imp.organization.key} ${imp.application.key}")
                None
            }
          }
        }
        loadImports(services ++ importedServices, cached)
      }
    }

    buildType match {
      case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiMiscEvent =>
        case class Aggregator(streams: Seq[KinesisStream] = Nil, cache: Map[String, Service] = Map.empty)
        val result = services.foldLeft(Aggregator()) { case (agg, service) =>
          val allServices = loadImports(Seq(service), agg.cache)
          val ms = MultiService(allServices.map(ApiBuilderService.apply).toList)
          val streams = processService(ms, service)
          Aggregator(agg.streams ++ streams, agg.cache ++ allServices.map(s => s.organization.key + s.application.key -> s))
        }
        saveDescriptor(buildType, StreamDescriptor(result.streams))
      case BuildType.Api | BuildType.ApiInternal | BuildType.ApiMisc | BuildType.ApiPartner => // do nothing
    }
  }

  private def processService(multiService: MultiService, service: Service): Seq[KinesisStream] = {
    service.unions.filter(u => u.name.endsWith("_event") && u.discriminator.isDefined).flatMap { union =>
      multiService.findType(
        defaultNamespace = service.namespace,
        typeName = union.name
      ) match {
        case None => {
          println(s"[ERROR] Unable to find union ${union.name} in service ${service.name}")
          None
        }
        case Some(typ: ApiBuilderType.Union) => {
          val className = s"${ApiBuilderUtils.toPackageName(typ.namespace, quoteKeywords = false)}.${ApiBuilderUtils.toClassName(typ.name, quoteKeywords = false)}"
          StreamNames(FlowEnvironment.Production).json(className) match {
            case None =>
              println(s"[ERROR] Unable to generate stream name for union ${typ.qualified} [$className]")
              None
            case Some(streamName) if BannedStreamNames.contains(streamName) =>
              println(s"Skipping banned stream $streamName")
              None
            case Some(streamName) =>
              val candidates = processUnion(multiService, typ, streamName).toList
              val upserted = candidates.collect { case u: EventType.Upserted => u }
              val deleted = candidates.collect { case d: EventType.Deleted => d }
              val pairs = pairUpEvents(upserted, deleted)
              if (pairs.isEmpty) {
                None
              } else {
                val serviceMajorVersion = VersionParser.parse(service.version).major.getOrElse(0L)
                val internal = if (service.name.contains("-internal-") && !union.name.contains("_internal_")) "internal_" else ""
                val shortName = s"${union.name}_${internal}v$serviceMajorVersion"
                val allModels = multiService.allModels.map(_.model)
                val allUnions = multiService.allUnions.map(_.union)
                val allEnums = multiService.allEnums.map(_.enum)
                Some(KinesisStream(streamName, shortName, pairs, allModels, allUnions, allEnums))
              }
          }
        }
        case Some(typ) => {
          println(s"[ERROR] Expected the type of ${typ.qualified} to be a union and not a[${typ.getClass.getName}]")
          None
        }

      }
    }
  }

  private val UnionMemberRx = "(.*)_(upserted|deleted)_?(.*)".r

  private def processUnion(multiService: MultiService, apiBuilderUnion: ApiBuilderType.Union, streamName: String): Seq[EventType] = {
    apiBuilderUnion.union.types.flatMap { member =>
      val types = multiService.findType(
        defaultNamespace = apiBuilderUnion.service.namespace,
        typeName = member.`type`
      )
      if (types.isEmpty) {
        println(s"[ERROR] Unable to find model for union ${apiBuilderUnion.qualified} member ${member.`type`}")
      }
      types.toSeq.flatMap {
        case m: ApiBuilderType.Model =>
          processModel(multiService, apiBuilderUnion, member, m)
        case u: ApiBuilderType.Union =>
          processUnion(multiService, u, streamName)
        case ApiBuilderType.Enum(_, enum) =>
          println(s"[ERROR] Don't know what to do with an union ${apiBuilderUnion.qualified} member $enum of type enum")
          Nil
        case other =>
          println(s"[ERROR] Don't know what to do with an union ${apiBuilderUnion.qualified} member $other")
          Nil
      }
    }
  }

  private def processModel(multiService: MultiService, apiBuilderUnion: ApiBuilderType.Union, unionMember: UnionType, apiBuilderModel: ApiBuilderType.Model): Seq[EventType] = {
    val discriminator = unionMember.discriminatorValue.getOrElse(unionMember.`type`)
    apiBuilderModel.name match {
      case UnionMemberRx(typeName, eventType, _) =>
        if (eventType == "upserted") {
          val payloadField = apiBuilderModel.model.fields.find(EventUnionTypeMatcher.matchFieldToPayloadType(_, typeName))
          if (payloadField.isEmpty) {
            println(s"Skipping non v2 upserted union ${apiBuilderUnion.qualified} member ${apiBuilderModel.qualified}: field not found")
          }
          val payloadTypes = payloadField.toSeq.flatMap(pf => extractPayloadModels(apiBuilderModel, pf, multiService))
          if (payloadTypes.isEmpty) {
            println(s"Skipping non v2 upserted union ${apiBuilderUnion.qualified} member ${apiBuilderModel.qualified}: payload type not found")
          }
          for {
            pt <- payloadTypes
            fld <- payloadField.toSeq
          } yield {
            EventType.Upserted(apiBuilderModel.name, typeName, fld.name, pt.model, discriminator)
          }
        } else {
          val idField = apiBuilderModel.model.fields.find(f => f.name == "id" && f.`type` == "string")
          val payloadField = apiBuilderModel.model.fields.find(EventUnionTypeMatcher.matchFieldToPayloadType(_, typeName))
          val payloadTypes = payloadField.toSeq.flatMap(pf => extractPayloadModels(apiBuilderModel, pf, multiService))
          if (payloadTypes.isEmpty && idField.isDefined) {
            Seq(EventType.Deleted(apiBuilderModel.name, typeName, None, discriminator))
          } else if (payloadTypes.nonEmpty) {
            payloadTypes.map { pt => EventType.Deleted(apiBuilderModel.name, typeName, Some(pt.model), discriminator) }
          } else {
            println(s"Skipping non v2 deleted union ${apiBuilderUnion.qualified} member ${apiBuilderModel.qualified}")
            Nil
          }
        }
      case _ =>
        println(s"Skipping misnamed union ${apiBuilderUnion.qualified} member ${apiBuilderModel.qualified}")
        Nil
    }
  }

  private def extractPayloadModels(model: ApiBuilderType.Model, typeField: Field, multiService: MultiService): Option[ApiBuilderType.Model] = {
    multiService.findType(
      defaultNamespace = model.namespace,
      typeName = typeField.`type`
    ) match {
      case Some(m: ApiBuilderType.Model) => Some (m)
      case _ => None
    }
  }

  private def pairUpEvents(upserted: List[EventType.Upserted], deleted: List[EventType.Deleted]): List[CapturedType] = {
    upserted match {
      case head :: tail =>
        deleted.find(_.typeName == head.typeName).fold {
          println(s"Skipping unpaired v2 upserted member ${head.eventName}")
          pairUpEvents(tail, deleted)
        }{ d =>
          List(CapturedType(head.fieldName, head.typeName, head.payloadType, head.discriminator, d.discriminator, d.payloadType.isDefined)) ++ pairUpEvents(tail, deleted.filterNot(_ == d))
        }
      case Nil =>
        deleted.foreach { d =>
          println(s"Skipping unpaired v2 deleted member ${d.eventName}")
        }
        Nil
    }
  }

  @silent def saveDescriptor(buildType: BuildType, descriptor: StreamDescriptor): Unit = {
    import play.api.libs.json._
    import io.apibuilder.spec.v0.models.json._
    implicit val w1: Writes[CapturedType] = Json.writes[CapturedType]
    implicit val w2: Writes[KinesisStream] = Json.writes[KinesisStream]
    implicit val w3: Writes[StreamDescriptor] = Json.writes[StreamDescriptor]
    val path = s"/tmp/flow-$buildType-streams.json"
    new java.io.PrintWriter(path) {
      write(Json.prettyPrint(Json.toJson(descriptor)))
      close()
    }
    println(s"Stream info file created. See: $path")
  }

}