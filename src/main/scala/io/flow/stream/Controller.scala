package io.flow.stream

import io.apibuilder.spec.v0.models.{Field, Model, Service, UnionType}
import io.apibuilder.validation.{ApiBuilderService, ApiBuilderType, MultiService}
import io.flow.build.{Application, BuildConfig, BuildType, DownloadCache}
import io.flow.util.{FlowEnvironment, StreamNames, VersionParser}

import java.nio.file.Path
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

case class Controller() extends io.flow.build.Controller {
  private val BannedStreamNames = Set(
    "production.local.item.event.v0.local_item_event.json",
    "production.localized.item.internal.event.v0.localized_item_event.json",
  )

  private val ArrayTypeMatcher = """\[(.+)\]""".r

  override val name = "Stream"
  override val command = "stream"

  override def run(
    buildType: BuildType,
    buildConfig: BuildConfig,
    downloadCache: DownloadCache,
    services: Seq[Service],
  )(implicit
    ec: ExecutionContext,
  ): Unit = {

    @tailrec
    def loadImports(services: Seq[Service], cached: Map[String, Service]): Seq[Service] = {
      val imports = services.flatMap(_.imports).groupBy(_.uri).values.toList.map(_.last)
      val filteredImports = imports.filterNot(imp =>
        services.exists(svc =>
          svc.organization.key == imp.organization.key && svc.application.key == imp.application.key,
        ),
      )
      if (filteredImports.isEmpty) {
        services
      } else {
        val importedServices = filteredImports.flatMap { imp =>
          cached.get(imp.organization.key + imp.application.key).orElse {
            downloadCache.downloadService(Application.latest(imp.organization.key, imp.application.key)) match {
              case Right(svc) => Some(svc)
              case Left(errors) =>
                println(
                  s"[ERROR] Failed to fetch import ${imp.organization.key} ${imp.application.key}: ${errors.mkString(", ")}",
                )
                None
            }
          }
        }
        loadImports(services ++ importedServices, cached)
      }
    }

    if (buildType.isEvent) {
      // warm the cache
      downloadCache.downloadAllServicesAndImports(services)

      case class Aggregator(streams: Seq[KinesisStream] = Nil, cache: Map[String, Service] = Map.empty)
      val result = services.foldLeft(Aggregator()) { case (agg, service) =>
        val allServices = loadImports(Seq(service), agg.cache)
        val ms = MultiService(allServices.map(ApiBuilderService.apply).toList)
        val streams = processService(ms, service)
        Aggregator(
          agg.streams ++ streams,
          agg.cache ++ allServices.map(s => s.organization.key + s.application.key -> s),
        )
      }
      saveDescriptor(buildType, buildConfig.output, StreamDescriptor(result.streams))
    }
  }

  private def processService(multiService: MultiService, service: Service): Seq[KinesisStream] = {
    service.unions.filter(u => u.name.endsWith("_event") && u.discriminator.isDefined).flatMap { union =>
      multiService.findType(
        defaultNamespace = service.namespace,
        typeName = union.name,
      ) match {
        case None => {
          println(s"[ERROR] Unable to find union ${union.name} in service ${service.name}")
          None
        }
        case Some(typ: ApiBuilderType.Union) => {
          val className = s"${ApiBuilderUtils.toPackageName(typ.namespace, quoteKeywords = false)}.${ApiBuilderUtils
              .toClassName(typ.name, quoteKeywords = false)}"
          StreamNames(FlowEnvironment.Production).json(className) match {
            case None =>
              println(s"[ERROR] Unable to generate stream name for union ${typ.qualified} [$className]")
              None
            case Some(streamName) if BannedStreamNames.contains(streamName) =>
              None
            case Some(streamName) =>
              val candidates = processUnion(multiService, typ, streamName).toList
              val inserted = candidates.collect { case i: EventType.Inserted => i }
              val updated = candidates.collect { case u: EventType.Updated => u }
              val upserted = candidates.collect { case u: EventType.Upserted => u }
              val deleted = candidates.collect { case d: EventType.Deleted => d }
              val pairs = pairUpEvents(inserted, updated, upserted, deleted)
              if (pairs.isEmpty) {
                None
              } else {
                val serviceMajorVersion = VersionParser.parse(service.version).major.getOrElse(0L)
                val internal =
                  if (service.name.contains("-internal-") && !union.name.contains("_internal_")) "internal_" else ""
                val shortName = s"${union.name}_${internal}v$serviceMajorVersion"
                val allModels = multiService.allModels.map(_.model)
                val allUnions = multiService.allUnions.map(_.union)
                val allEnums = multiService.allEnums.map(_.enum)
                Some(KinesisStream(streamName, shortName, pairs, allModels, allUnions, allEnums))
              }
          }
        }
        case Some(typ) => {
          println(s"[ERROR] Expected the type of ${typ.name} to be a union and not a[${typ.getClass.getName}]")
          None
        }

      }
    }
  }

  private[stream] val UnionMemberRx = "(.*)_(upserted|inserted|updated|deleted)_?(.*)".r

  private def processUnion(
    multiService: MultiService,
    apiBuilderUnion: ApiBuilderType.Union,
    streamName: String,
  ): Seq[EventType] = {
    apiBuilderUnion.union.types.flatMap { member =>
      val types = multiService.findType(
        defaultNamespace = apiBuilderUnion.service.namespace,
        typeName = member.`type`,
      )
      if (types.isEmpty) {
        println(s"[ERROR] Unable to find model for union ${apiBuilderUnion.qualified} member ${member.`type`}")
      }
      types.toSeq.flatMap {
        case m: ApiBuilderType.Model =>
          processModel(multiService, member, m)
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

  private def processModel(
    multiService: MultiService,
    unionMember: UnionType,
    apiBuilderModel: ApiBuilderType.Model,
  ): Seq[EventType] = {
    val discriminator = unionMember.discriminatorValue.getOrElse(unionMember.`type`)
    apiBuilderModel.name match {
      case UnionMemberRx(typeName, "inserted", _) =>
        processUpsertLike(multiService, apiBuilderModel, typeName, discriminator)(EventType.Inserted.apply)
      case UnionMemberRx(typeName, "updated", _) =>
        processUpsertLike(multiService, apiBuilderModel, typeName, discriminator)(EventType.Updated.apply)
      case UnionMemberRx(typeName, "upserted", _) =>
        processUpsertLike(multiService, apiBuilderModel, typeName, discriminator)(EventType.Upserted.apply)
      case UnionMemberRx(typeName, "deleted", _) =>
        val eventIdField = findIdField(apiBuilderModel)
        val payloadField = apiBuilderModel.model.fields.find(EventUnionTypeMatcher.matchFieldToPayloadType(_, typeName))
        val payloadTypes = payloadField.toSeq.flatMap(pf => extractPayloadModels(apiBuilderModel, pf, multiService))
        if (payloadTypes.isEmpty && eventIdField.isDefined) {
          Seq(EventType.Deleted(apiBuilderModel.name, typeName, None, eventIdField.get, discriminator))
        } else if (payloadTypes.nonEmpty) {
          for {
            pt <- payloadTypes
            idField <- findIdField(pt)
          } yield {
            EventType.Deleted(apiBuilderModel.name, typeName, Some(pt.model), idField, discriminator)
          }
        } else {
          // println(s"Skipping non v2 deleted union ${apiBuilderUnion.qualified} member ${apiBuilderModel.qualified}")
          Nil
        }
      case _ =>
        // println(s"Skipping misnamed union ${apiBuilderUnion.qualified} member ${apiBuilderModel.qualified}")
        Nil
    }
  }

  private def processUpsertLike(
    multiService: MultiService,
    apiBuilderModel: ApiBuilderType.Model,
    typeName: String,
    discriminator: String,
  )(
    build: (String, String, String, Model, Field, String) => EventType.UpsertLike,
  ): Seq[EventType.UpsertLike] = {
    val payloadField =
      apiBuilderModel.model.fields.find(EventUnionTypeMatcher.matchFieldToPayloadType(_, typeName)).toSeq
    val payloadTypes = payloadField.flatMap(pf => extractPayloadModels(apiBuilderModel, pf, multiService))
    for {
      pt <- payloadTypes
      fld <- payloadField
      idField <- findIdField(pt)
    } yield {
      build(apiBuilderModel.name, typeName, fld.name, pt.model, idField, discriminator)
    }
  }

  private def findIdField(model: ApiBuilderType.Model): Option[Field] = {
    def findFieldWithName(name: String) = model.model.fields.find(f => f.name == name && f.`type` == "string")
    findFieldWithName("id")
      .orElse(
        findFieldWithName("key"),
      )
      .orElse(
        findFieldWithName("number"),
      )
  }

  private def extractPayloadModels(
    model: ApiBuilderType.Model,
    typeField: Field,
    multiService: MultiService,
  ): Option[ApiBuilderType.Model] = {
    multiService.findType(
      defaultNamespace = model.namespace,
      typeName = typeField.`type`,
    ) match {
      case Some(m: ApiBuilderType.Model) => Some(m)
      case Some(m: ApiBuilderType.Union) => sythesizeModelFromUnion(m, multiService)
      case _ => None
    }
  }

  private def sythesizeModelFromUnion(
    union: ApiBuilderType.Union,
    multiService: MultiService,
  ): Option[ApiBuilderType.Model] = {
    val unionModels = union.types.flatMap { unionType =>
      multiService.findType(
        defaultNamespace = union.namespace,
        typeName = unionType.`type`.`type`,
      ) collect {
        case m: ApiBuilderType.Model => Some(m)
        case m: ApiBuilderType.Union => sythesizeModelFromUnion(m, multiService)
      }
    }.flatten
    unionModels.headOption.map { head =>
      mergeModels(unionModels, union.name, head.service)
    }
  }

  private def mergeModels(
    models: Seq[ApiBuilderType.Model],
    toName: String,
    service: ApiBuilderService,
  ): ApiBuilderType.Model = {
    val allFields: Seq[Map[String, Field]] = models.map(_.model.fields).map { _.map(t => t.name -> t).toMap }

    val allFieldNames = allFields.foldLeft(Set.empty[String]) { case (agg, fields) => agg ++ fields.keySet }

    val mergedFields = allFieldNames.flatMap { fieldName =>
      val fields = allFields.flatMap(_.get(fieldName))
      val types = fields.map(_.`type`).toSet.toList
      val requireds = fields.map(_.required).toSet

      types match {
        case _ :: Nil if fields.size == models.size && requireds.size == 1 =>
          // field present in all models, same type and same requiredness
          fields.headOption
        case _ :: Nil =>
          // field present in some or all models, same type
          fields.headOption.map(_.copy(required = false))
        case _ =>
          // different types, hacky-hack to use obj
          val (arrayTypes, singularTypes) = types.partition(ArrayTypeMatcher.matches)

          if (arrayTypes.size > 0 && singularTypes.size > 0) {
            // mix of arrays and singular types - can't support that, drop the field
            None
          } else if (arrayTypes.size > 0 && requireds.size == 1) {
            // Only arrays, all same requiredness
            fields.headOption.map(f => f.copy(`type` = "[obj]"))
          } else if (arrayTypes.size > 0) {
            // Only arrays, different requiredness
            fields.headOption.map(f => f.copy(`type` = "[obj]", required = false))
          } else if (requireds.size == 1) {
            // Only singular types, all same requiredness
            fields.headOption.map(f => f.copy(`type` = "obj"))
          } else {
            // Only singular types, different requiredness
            fields.headOption.map(f => f.copy(`type` = "obj", required = false))
          }
      }
    }

    val model = Model(
      name = toName,
      plural = toName,
      description = None,
      deprecation = None,
      fields = mergedFields.toSeq,
      attributes = Nil,
      interfaces = Nil,
    )

    ApiBuilderType.Model(service, model)
  }

  private[stream] def pairUpEvents(
    inserted: List[EventType.Inserted],
    updated: List[EventType.Updated],
    upserted: List[EventType.Upserted],
    deleted: List[EventType.Deleted],
  ): List[CapturedType] = {
    // Merge inserted/updated events with the same typeName - they share the same deleted event
    val mergedInsertedUpdated: List[List[EventType.UpsertLike]] = (inserted ++ updated)
      .groupBy(u => (u.typeName, u.idField.name))
      .values
      .toList

    // Upserted events are kept as-is (not merged with other events)
    val allGroups: List[List[EventType.UpsertLike]] = mergedInsertedUpdated ++ upserted.map(List(_))

    pairWithDeleted(allGroups, deleted)
  }

  private[stream] def pairWithDeleted(
    upsertGroups: List[List[EventType.UpsertLike]],
    deleted: List[EventType.Deleted],
  ): List[CapturedType] = {
    upsertGroups match {
      case group :: tail =>
        val head = group.head
        val candidates = deleted.filter(d => d.typeName == head.typeName && d.idField.name == head.idField.name)
        val candidate = candidates.find(_.payloadType.isDefined).orElse(candidates.headOption)
        candidate.fold {
          group.foreach(u => println(s"Skipping unpaired v2 upserted member ${u.eventName}"))
          pairWithDeleted(tail, deleted)
        } { d =>
          List(
            CapturedType(
              head.fieldName,
              head.typeName,
              head.payloadType,
              group.map(_.discriminator),
              d.discriminator,
              d.payloadType.isDefined,
            ),
          ) ++ pairWithDeleted(tail, deleted.filterNot(_ == d))
        }
      case Nil =>
        deleted.foreach { d =>
          println(s"Skipping unpaired v2 deleted member ${d.eventName}")
        }
        Nil
    }
  }

  def saveDescriptor(buildType: BuildType, output: Path, descriptor: StreamDescriptor): Unit = {
    import play.api.libs.json._
    import io.apibuilder.spec.v0.models.json._

    // Custom Writes for CapturedType to output both upsertedDiscriminator (for backward compatibility)
    // and upsertedDiscriminators (new format). Can remove upsertedDiscriminator once all clients updated.
    implicit val w1: Writes[CapturedType] = (ct: CapturedType) =>
      Json.obj(
        "fieldName" -> ct.fieldName,
        "typeName" -> ct.typeName,
        "modelType" -> ct.modelType,
        "upsertedDiscriminator" -> ct.upsertedDiscriminators.head,
        "deletedDiscriminator" -> ct.deletedDiscriminator,
        "deletedHasModel" -> ct.deletedHasModel,
        "upsertedDiscriminators" -> ct.upsertedDiscriminators,
      )
    implicit val w2: Writes[KinesisStream] = Json.writes[KinesisStream]
    implicit val w3: Writes[StreamDescriptor] = Json.writes[StreamDescriptor]
    val path = output.resolve(s"flow-$buildType-streams.json").toFile
    new java.io.PrintWriter(path) {
      write(Json.prettyPrint(Json.toJson(descriptor)))
      close()
    }
    println(s"Stream info file created. See: $path")
  }

}
