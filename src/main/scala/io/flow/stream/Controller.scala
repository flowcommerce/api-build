package io.flow.stream
import io.apibuilder.spec.v0.models.{Field, Model, Service, Union}
import io.apibuilder.validation.{ApiBuilderService, ApibuilderType, MultiService}
import io.flow.build.{Application, BuildType, Downloader}
import io.flow.util.{FlowEnvironment, StreamNames, VersionParser}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

case class Controller() extends io.flow.build.Controller {
  private val BannedStreamNames = Set("production.local.item.event.v0.local_item_event.json", "production.localized.item.internal.event.v0.localized_item_event.json")

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
          val ms = MultiService(allServices.map(ApiBuilderService.apply))
          val streams = processService(ms, service)
          Aggregator(agg.streams ++ streams, agg.cache ++ allServices.map(s => s.organization.key + s.application.key -> s))
        }
        saveDescriptor(buildType, StreamDescriptor(result.streams))
      case BuildType.Api | BuildType.ApiInternal | BuildType.ApiMisc | BuildType.ApiPartner => // do nothing
    }
  }

  private def processService(multiService: MultiService, service: Service): Seq[KinesisStream] = {
    service.unions.filter(u => u.name.endsWith("_event") && u.discriminator.isDefined).flatMap { union =>
      val types = multiService.findType(union.name)
      if (types.size == 0) {
        println(s"[ERROR] Unable to find union ${union.name}")
      }
      types.flatMap { typ =>
        val className = s"${ApiBuilderUtils.toPackageName(typ.service.namespace, quoteKeywords = false)}.${ApiBuilderUtils.toClassName(union.name, quoteKeywords =false)}"
        StreamNames(FlowEnvironment.Production).json(className) match {
          case None =>
            println(s"[ERROR] Unable to generate stream name for union ${union.name} [$className]")
            None
          case Some(streamName) if BannedStreamNames.contains(streamName) =>
            println(s"Skipping banned stream $streamName")
            None
          case Some(streamName) =>
            val candidates = processUnion(multiService, union, streamName).toList
            val upserted = candidates.collect { case u: EventType.Upserted => u }
            val deleted  = candidates.collect { case d: EventType.Deleted => d }
            val pairs = pairUpEvents(upserted, deleted)
            if (pairs.isEmpty) {
              None
            } else {
              val serviceMajorVersion = VersionParser.parse(service.version).major.getOrElse(0)
              val internal = if (service.name.contains("-internal-") && !union.name.contains("_internal_")) "internal_" else ""
              val shortName = s"${union.name}_${internal}v$serviceMajorVersion"
              val allModels = multiService.services.flatMap(_.service.models)
              val allUnions = multiService.services.flatMap(_.service.unions)
              val allEnums = multiService.services.flatMap(_.service.enums)
              Some(KinesisStream(streamName, shortName, pairs, allModels, allUnions, allEnums))
            }
        }
      }
    }
  }

  private val UnionMemberRx = "(.*)_(upserted|deleted)_?(.*)".r

  private def processUnion(multiService: MultiService, union: Union, streamName: String): Seq[EventType] = {
    union.types.flatMap { member =>
      val types = multiService.findType(member.`type`)
      if (types.size == 0) {
        println(s"[ERROR] Unable to find model for union ${union.name} member ${member.`type`}")
      }
      types.flatMap {
        case ApibuilderType.Model(_, model) =>
          val discriminator = member.discriminatorValue.getOrElse(member.`type`)
          model.name match {
            case UnionMemberRx(typeName, eventType, version) =>
              if (eventType == "upserted") {
                val payloadField = model.fields.find(matchFieldToPayloadType(_, typeName, version))
                payloadField.fold {
                  println(s"Skipping non v2 upserted union ${union.name} member ${model.name}: field not found")
                }{_ => }
                val payloadType = payloadField.flatMap(pf => extractPayloadModel(model.fields, pf, version, multiService))
                payloadType.fold {
                  println(s"Skipping non v2 upserted union ${union.name} member ${model.name}: payload type not found")
                }{_ => }
                for {
                  fld <- payloadField.toSeq
                  pt <- payloadType
                } yield (
                  EventType.Upserted(model.name, typeName, fld.name, pt, discriminator)
                )
              } else {
                val idField = model.fields.find(f => f.name == "id" && f.`type` == "string")
                val payloadField = model.fields.find(matchFieldToPayloadType(_, typeName, version))
                val payloadType = payloadField.flatMap(pf => extractPayloadModel(model.fields, pf, version, multiService))
                if (idField.isDefined || payloadType.isDefined) {
                  Seq(EventType.Deleted(model.name, typeName, payloadType, discriminator))
                } else {
                  println(s"Skipping non v2 deleted union ${union.name} member ${model.name}")
                  Nil
                }
              }
            case _ =>
              println(s"Skipping misnamed union ${union.name} member ${model.name}")
              Nil
          }
        case ApibuilderType.Union(_, nestedUnion) =>
          processUnion(multiService, nestedUnion, streamName)
        case ApibuilderType.Enum(_, enum) =>
          println(s"[ERROR] Don't know what to do with an union ${union.name} member $enum of type enum")
          Nil
        case other =>
          println(s"[ERROR] Don't know what to do with an union ${union.name} member $other")
          Nil
      }
    }
  }

  private def extractPayloadModel(fields: Seq[Field], typeField: Field, version: String, multiService: MultiService): Option[Model] = {
    for {
      payloadType: ApibuilderType <- multiService.findType(typeField.`type`).headOption
      model <- payloadType match {
        case ApibuilderType.Model(_, model) => Some(model)
        case _ => None
      }
    } yield model
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

  private def matchFieldToPayloadType(field: Field, typeName: String, version: String): Boolean = {
    matchFileName(typeName, field.name) && matchFieldType(typeName, field.`type`, version)
  }

  private def matchFileName(typeName: String, fieldName: String) = {
    typeName.equals(fieldName) || typeName.endsWith(s"_$fieldName")
  }

  private def matchFieldType(typeName: String, fieldType: String, version: String) = {
    val simpleType = fieldType.reverse.takeWhile(_ != '.').reverse
    typeName.equals(simpleType) || typeName.endsWith(s"_$simpleType") || fieldType.endsWith(s".${typeName}_$version")
  }

  private def saveDescriptor(buildType: BuildType, descriptor: StreamDescriptor): Unit = {
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