package io.flow.stream
import io.apibuilder.spec.v0.models.{Field, Model, Service, Union}
import io.apibuilder.validation.{ApiBuilderService, ApibuilderType, MultiService}
import io.flow.build.{Application, BuildType, Downloader}
import io.flow.util.{FlowEnvironment, StreamNames, VersionParser}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

case class Controller() extends io.flow.build.Controller {
  override val name = "Stream"
  override val command = "stream"


  override def run(buildType: BuildType, downloader: Downloader, services: Seq[Service])(implicit ec: ExecutionContext): Unit = {

    @tailrec
    def loadImports(services: Seq[Service], cached: Map[String, Service]): Seq[Service] = {
      val imports = services.flatMap(_.imports).groupBy(_.uri).values.toList.map(_.sortBy(_.version).last)
      val filteredImports = imports.filterNot(imp => services.exists(svc => svc.organization.key == imp.organization.key && svc.application.key == imp.application.key))
      if (filteredImports.size == 0) {
        services
      } else {
        val importedServices = filteredImports.flatMap { imp =>
          cached.get(imp.organization.key + imp.application.key + imp.version).orElse {
            println("Loading imports...")
            downloader.service(Application(imp.organization.key, imp.application.key, imp.version)) match {
              case Right(svc) => Some(svc)
              case Left(_) =>
                println(s"Error fetching import $imp")
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
        val agg = services.foldLeft(Aggregator()) { case (agg, service) =>
          val allServices = loadImports(Seq(service), agg.cache)
          val ms = MultiService(allServices.map(ApiBuilderService.apply))
          val streams = processService(ms, service)
          Aggregator(agg.streams ++ streams, agg.cache ++ allServices.map(s => s.organization.key + s.application.key + s.version -> s))
        }
        saveDescriptor(buildType, StreamDescriptor(agg.streams))
      case BuildType.Api | BuildType.ApiInternal | BuildType.ApiMisc | BuildType.ApiPartner => // do nothing
    }
  }

  private def processService(multiService: MultiService, service: Service): Seq[KinesisStream] = {
    service.unions.filter(u => u.name.endsWith("_event") && u.discriminator.isDefined).flatMap { union =>
      multiService.findType(union.name) match {
        case None =>
          println(s"Unable to find union ${union.name}")
          None
        case Some(typ) =>
          val className = s"${ApiBuilderUtils.toPackageName(typ.service.namespace, false)}.${ApiBuilderUtils.toClassName(union.name, false)}"
          StreamNames(FlowEnvironment.Production).json(className) match {
            case None =>
              println(s"Unable to generate stream name for union ${union.name} [$className]")
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
                val shortName = s"${union.name}_v${serviceMajorVersion}"
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
      multiService.findType(member.`type`) match {
        case None =>
          println(s"Unable to find model for union ${union.name} member ${member.`type`}")
          None
        case Some(ApibuilderType.Enum(ns, enum)) =>
          println(s"Don't know what to do with an union ${union.name} member $enum of type enum")
          None
        case Some(ApibuilderType.Model(ns, model)) =>
          val discriminator = member.discriminatorValue.getOrElse(member.`type`)
          model.name match {
            case UnionMemberRx(payloadName, eventType, version) =>
              if (eventType == "upserted") {
                val payloadType = extractPayloadModel(model.fields, payloadName, version, multiService)
                payloadType.fold {
                  println(s"Skipping non v2 upserted union ${union.name} member ${model.name}")
                  None: Option[EventType]
                } { payloadType =>
                  Some(EventType.Upserted(model.name, payloadName, payloadType, discriminator))
                }
              } else {
                val idField = model.fields.find(f => f.name == "id" && f.`type` == "string")
                val payloadType = extractPayloadModel(model.fields, payloadName, version, multiService)
                if (idField.isDefined || payloadType.isDefined) {
                  Some(EventType.Deleted(model.name, payloadName, payloadType, discriminator))
                } else {
                  println(s"Skipping non v2 deleted union ${union.name} member ${model.name}")
                  None
                }
              }
            case _ =>
              println(s"Skipping misnamed union ${union.name} member ${model.name}")
              None
          }
        case Some(ApibuilderType.Union(ns, union)) =>
          processUnion(multiService, union, streamName)
      }
    }
  }

  private def extractPayloadModel(fields: Seq[Field], payloadName: String, version: String, multiService: MultiService): Option[Model] = {
    for {
      typeField <- fields.find(matchFieldToPayloadType(_, payloadName, version))
      payloadType: ApibuilderType <- multiService.findType(typeField.`type`)
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
          List(CapturedType(head.typeName, head.payloadType, head.discriminator, d.discriminator, d.payloadType.isDefined)) ++ pairUpEvents(tail, deleted.filterNot(_ == d))
        }
      case Nil =>
        deleted.foreach { d =>
          println(s"Skipping unpaired v2 deleted member ${d.eventName}")
        }
        Nil
    }
  }

  private def matchFieldToPayloadType(field: Field, payloadName: String, version: String): Boolean = {
    payloadName.contains(field.name) && (field.`type`.endsWith(s".$payloadName") || field.`type`.endsWith(s".${payloadName}_$version"))
  }

  private def saveDescriptor(buildType: BuildType, descriptor: StreamDescriptor): Unit = {
    import play.api.libs.json._
    import io.apibuilder.spec.v0.models.json._
    implicit val w1 = Json.writes[CapturedType]
    implicit val w2 = Json.writes[KinesisStream]
    implicit val w3 = Json.writes[StreamDescriptor]
    val path = s"/tmp/flow-$buildType-streams.json"
    new java.io.PrintWriter(path) {
      write(Json.prettyPrint(Json.toJson(descriptor)))
      close
    }
    println(s"Stream info file created. See: $path")

  }

}