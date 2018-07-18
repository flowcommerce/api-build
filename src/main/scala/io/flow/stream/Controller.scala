package io.flow.stream
import io.apibuilder.spec.v0.models.{Field, Service, Union}
import io.apibuilder.validation.{ApiBuilderService, ApibuilderType, MultiService}
import io.flow.build.{Application, BuildType, Downloader}
import io.flow.util.{FlowEnvironment, StreamNames}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

case class Controller() extends io.flow.build.Controller {
  override val name = "Stream"
  override val command = "stream"


  override def run(buildType: BuildType, downloader: Downloader, services: Seq[Service])(implicit ec: ExecutionContext): Unit = {

    @tailrec
    def loadImports(services: Seq[Service]): Seq[Service] = {
      val imports = services.flatMap { service => service.imports }.groupBy(_.uri).values.toList.map(_.sortBy(_.version).last)
      val filteredImports = imports.filterNot(imp => services.exists(svc => svc.organization.key == imp.organization.key && svc.application.key == imp.application.key))
      if (filteredImports.size == 0) {
        services
      } else {
        println("Loading imports...")
        val importedServices = filteredImports.flatMap { imp =>
          downloader.service(Application(imp.organization.key, imp.application.key, imp.version)) match {
            case Right(svc) => Some(svc)
            case Left(err) =>
              println(s"Error fetching import $imp")
              None
          }
        }
        loadImports(services ++ importedServices)
      }
    }

    buildType match {
      case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiMiscEvent =>
        println(s"buildType=$buildType downloader=$downloader")
        val allServices = loadImports(services)
        val ms = MultiService(allServices.map(ApiBuilderService.apply))
        val streams = services.flatMap(processService(ms, _)).filterNot(_.capturedEvents.isEmpty)
        println(s"********\n${streams.mkString("\n")}")
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
          val className = s"${ApiBuilderUtils.toPackageName(typ.nameSpace, false)}.${ApiBuilderUtils.toClassName(union.name, false)}"
          StreamNames(FlowEnvironment.Current).json(className) match {
            case None =>
              println(s"Unable to generate stream name for union ${union.name} [$className]")
              None
            case Some(streamName) =>
              val candidates = processUnion(multiService, union, streamName).toList
              val upserted = candidates.collect { case u: EventType.Upserted => u }
              val deleted  = candidates.collect { case d: EventType.Deleted => d }
              val pairs = pairUpEvents(upserted, deleted)
              Some(KinesisStream(streamName, pairs))
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
                val typeField = model.fields.find(matchFieldToPayloadType(_, payloadName, version))
                val payloadType = typeField.flatMap { tf => multiService.findType(tf.`type`) }
                payloadType.fold {
                  println(s"Skipping non v2 upserted union ${union.name} member ${model.name}")
                  None: Option[EventType]
                } { payloadType =>
                  Some(EventType.Upserted(model.name, payloadName, payloadType, discriminator))
                }
              } else {
                val idField = model.fields.find(f => f.name == "id" && f.`type` == "string")
                val typeField = model.fields.find(matchFieldToPayloadType(_, payloadName, version))
                val payloadType = typeField.flatMap { tf => multiService.findType(tf.`type`) }
                idField.orElse(typeField).fold {
                  println(s"Skipping non v2 deleted union ${union.name} member ${model.name}")
                  None: Option[EventType]
                } { _ =>
                  Some(EventType.Deleted(model.name, payloadName, payloadType, discriminator))
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
    field.name == payloadName && (field.`type`.endsWith(s".$payloadName") || field.`type`.endsWith(s".${payloadName}_$version"))
  }
}