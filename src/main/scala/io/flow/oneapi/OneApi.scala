package io.flow.oneapi

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models._
import io.apibuilder.validation._
import io.flow.build.{BuildType, DownloadCache}
import play.api.libs.json.{JsString, Json}

private[oneapi] case class ContextualValue(context: String, value: String)

case class OneApi(
  buildType: BuildType,
  downloadCache: DownloadCache,
  originalServices: Seq[Service]
) {
  private[this] val MergeResourcePathsHack = Map(
    "organization" -> "/organizations",
    "timezone" -> "/",
    "io.flow.reference.v0.models.timezone" -> "/",
    "io.flow.common.v0.models.organization" -> "/organizations",
    "io.flow.external.paypal.v1.models.webhook_event" -> "/"
  )

  private[this] val canonical: ApiBuilderService = ApiBuilderService(originalServices.find(_.name == "common").getOrElse {
    originalServices.headOption.getOrElse {
      sys.error("Must have at least one service")
    }
  })

  private[this] val namespace = s"${buildType.namespace}.v" + majorVersion(canonical.service.version)

  private[this] val importedServices: List[ApiBuilderService] = downloadCache.downloadServices(
    originalServices.flatMap(_.imports).map { imp =>
      io.flow.build.Application.latest(imp.organization.key, imp.application.key)
    }.distinct.filterNot { a =>
      originalServices.exists { s => s.organization.key == a.organization && s.application.key == a.application }
    }
  ) match {
    case Left(errors) => sys.error(s"Failed to download imports: ${errors.mkString(", ")}")
    case Right(services) => services.map(ApiBuilderService(_)).toList
  }

  private[this] val services: List[ApiBuilderService] = originalServices.map(ApiBuilderService(_)).toList
  println(s"Services: ${services.map(_.service.namespace)}")
  private[this] val multiService: MultiService = MultiServiceImpl(services)
  private[this] val multiServiceWithImports: MultiService = MultiServiceImpl(services ++ importedServices)

  def process(): ValidatedNec[String, Service] = {
    (
      validatePaths(),
      validateRecordNames()
    ).mapN { case (_, _) =>
      buildOneApi()
    }
  }

  private[this] def buildOneApi(): Service = {
    // Annotations are not namespaced, they're global. For convenience, we'll collect them from
    // all imports and add them to the root service
    val allAnnotations = services.flatMap { _.service.imports.flatMap(_.annotations) }.distinctBy(_.name)

    val baseService = Service(
      apidoc = canonical.service.apidoc,
      name = buildType.name,
      organization = canonical.service.organization,
      application = Application(key = buildType.key),
      namespace = namespace,
      version = canonical.service.version,
      baseUrl = Some("https://api.flow.io"),
      description = canonical.service.description,
      info = Info(
        license = Some(License(name = "MIT", url = Some("http://opensource.org/licenses/MIT"))),
        contact = Some(Contact(name = Some("Flow Commerce"), email = Some("tech@flow.io")))
      ),
      headers = Nil,
      imports = Nil,
      attributes = Nil,
      annotations = allAnnotations,

      enums = multiService.allEnums.map(_.`enum`),
      models = multiService.allModels.map(updateModel).map(_.model),
      interfaces = multiService.allInterfaces.map(updateInterface).map(_.interface),
      unions = multiService.allUnions.map(_.union),

      resources = services.flatMap { s =>
        s.service.resources.map { r => updateResource(s, r) }
      }
    )

    val flattened = flattenTypes(baseService)
    val service = flattened.copy(
      imports = stripAnnotations(
        buildImports(flattened, originalServices.flatMap(_.imports))
      )
    )

    val sorted = service.copy(
      enums = service.enums.sortBy(_.name.toLowerCase),
      models = service.models.sortBy(_.name.toLowerCase),
      interfaces = service.interfaces.sortBy(_.name.toLowerCase),
      unions = service.unions.sortBy(_.name.toLowerCase),
      resources = mergeResources(service.resources) match {
        case Invalid(errors) => sys.error("Error merging resources: " + errors.toList.mkString("\n"))
        case Valid(r) => r.toList.sortBy(resourceSortKey)
      }
    )

    if (buildType.isEvent) {
      createEventService(sorted)
    } else {
      sorted
    }
  }

  private[this] def flattenTypes(service: Service): Service = {
    if (buildType.flattenTypes) {
      FlattenTypeNames(flattenedServices = services).rewrite(service)
    } else {
      service
    }
  }

  /**
   * Filter imports to the namespaces actually referenced in the service and ensure each import namespace
   * is specified exactly once.
   */
  private[this] def buildImports(baseService: Service, imports: Seq[Import]): Seq[Import] = {
    val allNamespaces = AllTypeNames.findNamespaces(baseService)
    val availableImports = imports.distinctBy(_.namespace)
    availableImports.filter { imp =>
      allNamespaces.contains(imp.namespace)
    }
  }

  private[this] def stripAnnotations(imports: Seq[Import]): Seq[Import] = {
    imports.map(_.copy(annotations = Nil))
  }

  @scala.annotation.tailrec
  private[this] def mergeResources(remaining: Seq[Resource], merged: Seq[Resource] = Nil): ValidatedNec[String, Seq[Resource]] = {
    remaining.toList match {
      case Nil => merged.validNec
      case one :: rest => {
        merged.find(_.`type` == one.`type`) match {
          case None => mergeResources(rest, merged ++ Seq(one))
          case Some(r) => {
            merge(r, one) match {
              case Invalid(errors) => errors.toList.mkString("\n").invalidNec
              case Valid(result) => mergeResources(rest, merged.filter(_.`type` != one.`type`) ++ Seq(result))
            }
          }
        }
      }
    }
  }

  /**
    * Merges the two resources:
    *   - Takes fields from the first resource if both provide (e.g. description)
    *   - If paths are different, raises an error
    */
  private[this] def merge(a: Resource, b: Resource): ValidatedNec[String, Resource] = {
    assert(a.`type` == b.`type`)

    // Ideally the resource paths are the same; if not we have to
    // explicitly choose the resource path we want. This will
    // influence method names in the code generators and thus
    // important to get right.
    val allPaths = Seq(a.path, b.path).flatten.distinct
    val attributeNames = a.attributes.map(_.name)

    val path: ValidatedNec[String, Option[String]] = allPaths.toList match {
      case Nil => None.validNec
      case one :: Nil => Some(one).validNec
      case multiple => {
        MergeResourcePathsHack.get(a.`type`) match {
          case Some(defaultPath) => {
            if (allPaths.contains(defaultPath)) {
              Some(defaultPath).validNec
            } else if (defaultPath == "/") {
              Some(defaultPath).validNec
            } else {
              s"Error while merging resources of type[${a.`type`}]: Default path $defaultPath is not specified on either resource[${allPaths}]".invalidNec
            }
          }
          case None => {
            val (internal, public) = multiple.partition(p => p.startsWith("/internal"))
            public match {
              case Nil => internal.headOption.validNec
              case one :: Nil => Some(one).validNec
              case multiplePublic => {
                (
                  s"Cannot merge resources of type[${a.`type`}] with more than one non internal path:\n" +
                  multiplePublic.sorted.mkString("  - ", "\n  - ", "\n") +
                  "To resolve - edit api-build:src/main/scala/io/flow/oneapi/OneApi.scala and update MergeResourcePathsHack to add an explicit path for this resource"
                ).invalidNec
              }
            }
          }
        }
      }
    }

    path.map { p =>
      a.copy(
        path = p,
        description = a.description match {
          case Some(desc) => Some(desc)
          case None => b.description
        },
        operations = a.operations ++ b.operations,
        attributes = a.attributes ++ b.attributes.filter(attr => !attributeNames.contains(attr.name))
      )
    }
  }

  /**
    * Given a string version number in semver, e.g. 1.2.3, returns the
    * major version number as an integer (e.g. 1)
    */
  private[this] def majorVersion(version: String): Int = {
    version.split("\\.").headOption.getOrElse {
      sys.error(s"Version[$version] must be in semver")
    }.toInt
  }

  private[this] def resourceSortKey(resource: Resource): String = {
    val docs = resource.attributes.find(_.name == "docs").getOrElse {
      sys.error(s"Resource ${resource.`type`} is missing the 'docs' attribute")
    }
    val moduleName = (docs.value \ "module").as[JsString].value

    Seq(
      10000 + Module.moduleSortIndex(moduleName),
      resource.`type`.toLowerCase
    ).mkString(":")
  }

  private[this] def validatePaths(): ValidatedNec[String, Unit] = {
    val allPaths: Seq[ContextualValue] = services.flatMap { s =>
      s.service.resources.flatMap { r =>
        r.operations.map { op =>
          ContextualValue(
            context = s"${s.name}:resource[${r.`type`}] ${op.method} ${op.path}",
            value = normalizePath(op.method, op.path)
          )
        }
      }
    }

    dups("path", allPaths)
  }

  private[this] def normalizePath(method: Method, path: String): String = {
    val pathParts: Seq[String] = path.split("[/.]").toSeq.map { name =>
      if (name.startsWith(":")) {
        // Use a standard name here as this is a pattern - doesn't
        // matter what name the developer actually assigned in terms
        // of how the path will be routed.
        ":var"
      } else {
        name
      }
    }

    method.toString + " " + pathParts.mkString("/")
  }

  private[this] def validateRecordNames(): ValidatedNec[String, Unit] = {
    dups(
      "record",
      multiService.allTypes.filter {
        case _: ApiBuilderType.Interface => false
        case _ => true
      }.map { s =>
        ContextualValue(
          context = s.qualified,
          value = s.name
        )
      }
    )
  }

  /**
    * Returns an error message if there are duplicate values
    */
  private[oneapi] def dups(label: String, values: Seq[ContextualValue]): ValidatedNec[String, Unit] = {
    values.groupBy(_.value.toLowerCase).filter { _._2.size > 1 }.keys.toSeq.sorted.map { dup =>
      val dupValues = values.filter { v => dup == v.value.toLowerCase }
      assert(dupValues.size >= 2, s"Could not find duplicates for value[$dup]")
      s"Duplicate $label[$dup] in: " + dupValues.map(_.context).sorted.mkString(", ")
    }.toList match {
      case Nil => ().validNec
      case errors => errors.mkString(", ").invalidNec
    }
  }

  private[this] def createEventService(service: Service): Service = {
    val event = createEventUnion(service.unions)
    val eventType = createEventTypeEnum(event)
    service.copy(
      enums = service.enums ++ Seq(eventType),
      unions = Seq(event)
    )
  }

  private[this] def createEventUnion(unions: Seq[Union]): Union = {
    unions.map(_.discriminator).distinct.toList match {
      case discriminator :: Nil => {
        val types = unions.flatMap { _.types }
        val duplicates = types.map(_.`type`).groupBy(identity).collect { case (typ, instances) if instances.length > 1 => typ }
        assert(
          duplicates.isEmpty,
          s"Generated event union has duplicate types: ${duplicates.mkString(", ")}"
        )

        Union(
          name = "event",
          plural = "events",
          discriminator = discriminator,
          types = types,
          deprecation = None,
          attributes = Seq(docsAttribute(Module.Webhook))
        )
      }

      case discriminators => {
        sys.error("Service must have exactly 1 discriminator, but we found: " + discriminators.mkString(", "))
      }
    }
  }

  private[this] def createEventTypeEnum(event: Union): Enum = {
    Enum(
      name = "event_type",
      plural = "event_types",
      values = event.types.map { t =>
        EnumValue( 
          name = t.`type`
        )
      }.distinct,
      deprecation = None,
      attributes = Seq(docsAttribute(Module.Webhook))
    )
  }

  private[this] def updateResource(service: ApiBuilderService, resource: Resource): Resource = {
    updateDescription(
      service,
      updateOperations(
        ensureDocsAttribute(service, resource)
      )
    )
  }

  private[this] def updateDescription(service: ApiBuilderService, resource: Resource): Resource = {
    resource.copy(
      description = resource.description.orElse {
        multiServiceWithImports.findType(service.namespace, resource.`type`) match {
          case None => None
          case Some(t) => t match {
            case _: ScalarType => None
            case t: ApiBuilderType.Enum => t.enum.description
            case t: ApiBuilderType.Interface => t.interface.description
            case t: ApiBuilderType.Model => t.model.description
            case t: ApiBuilderType.Union => t.union.description
          }
        }
      }
    )
  }

  private[this] def updateOperations(resource: Resource): Resource = {
    sortOperations(
      resource.copy(
        operations = resource.operations.map(updateOperation),
      )
    )
  }

  private[this] def updateOperation(op: Operation): Operation = {
    op.copy(
      parameters = op.parameters.map(updateParameter),
      responses = op.responses.map(updateResponse)
    )
  }

  private[this] def updateParameter(param: Parameter): Parameter = {
    param.copy(
      description = param.description.orElse {
        Defaults.ParameterDescriptions.get(param.name)
      }
    )
  }

  private[this] def updateResponse(response: Response): Response = {
    response.copy(
      description = response.description.orElse {
        Defaults.ResponseDescriptions.get(responseCodeToString(response.code))
      }
    )
  }

  private[this] def responseCodeToString(code: ResponseCode): String = {
    code match {
      case ResponseCodeInt(c) => c.toString
      case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => "*"
    }
  }

  private[this] def sortOperations(resource: Resource): Resource = {
    resource.copy(
      operations = resource.operations.sortBy(OperationSort.key)
    )
  }

  private[this] def ensureDocsAttribute(service: ApiBuilderService, resource: Resource): Resource = {
    resource.attributes.find(_.name == "docs") match {
      case Some(_) => resource
      case None => {
        val module = Module.findByServiceName(service.name).getOrElse {
          Module.General
        }
        resource.copy(
          attributes = resource.attributes ++ Seq(docsAttribute(module))
        )
      }
    }

  }

  private[this] def docsAttribute(module: Module): Attribute = Attribute(
    name = "docs",
    value = Json.obj(
      "module" -> module.name
    )
  )

  private[this] def updateInterface(interface: ApiBuilderType.Interface): ApiBuilderType.Interface = {
    interface.copy(
      interface = interface.interface.copy(
        fields = interface.interface.fields.map(updateField)
      )
    )
  }

  private[this] def updateModel(model: ApiBuilderType.Model): ApiBuilderType.Model = {
    model.copy(
      model = model.model.copy(
        fields = model.model.fields.map(updateField)
      )
    )
  }

  private[this] def updateField(field: Field): Field = {
    field.copy(
      description = field.description.orElse {
        Defaults.FieldDescriptions.get(field.name)
      }
    )
  }
}
