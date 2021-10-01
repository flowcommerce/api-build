package io.flow.oneapi

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.rewriter.TypeRewriter
import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.{ApiBuilderService, MultiService, MultiServiceImpl}
import io.flow.build.BuildType
import play.api.libs.json.{JsString, Json}

private[oneapi] case class ContextualValue(context: String, value: String)

case class OneApi(
  buildType: BuildType,
  originalServices: Seq[Service]
) {
  private[this] val services: List[ApiBuilderService] = originalServices.map(ApiBuilderService(_)).toList
  private[this] val multiService: MultiService = MultiServiceImpl(services)

  private[this] val MergeResourcePathsHack = Map(
    "organization" -> "/organizations",
    "timezone" -> "/",
    "io.flow.reference.v0.models.timezone" -> "/",
    "io.flow.common.v0.models.organization" -> "/organizations",
    "io.flow.external.paypal.v1.models.webhook_event" -> "/"
  )

  private[this] val canonical: ApiBuilderService = multiService.services().find(_.name == "common").getOrElse {
    multiService.services().headOption.getOrElse {
      sys.error("Must have at least one service")
    }
  }

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

    val flattenedService = TypeRewriter { t =>
      t
    }.rewrite(multiService)

    val baseService = Service(
      apidoc = canonical.service.apidoc,
      name = buildType.name,
      organization = canonical.service.organization,
      application = Application(
        key = buildType.key
      ),
      namespace = s"${buildType.namespace}.v" + majorVersion(canonical.service.version),
      version = canonical.service.version,
      baseUrl = Some(
        canonical.service.baseUrl.getOrElse {
          "https://api.flow.io"
        }
      ),
      description = canonical.service.description,
      info = canonical.service.info,
      headers = Nil,
      imports = Nil,
      attributes = Nil,

      enums = flattenedService.allEnums.map(_.`enum`).sortBy { _.name.toLowerCase },
      models = flattenedService.allModels.map(_.model).sortBy(_.name.toLowerCase ),
      interfaces = flattenedService.allInterfaces.map(_.interface).sortBy(_.name.toLowerCase ),
      unions = flattenedService.allUnions.map(_.union).sortBy(_.name.toLowerCase ),

      resources = mergeResources(
        services.flatMap { s =>
          s.service.resources
        }
      ) match {
        case Invalid(errors) => sys.error(errors.toList.mkString("\n"))
        case Valid(r) => r.toList.sortBy { resourceSortKey }
      },

      annotations = allAnnotations
    )

    val service = baseService.copy(
      imports = stripAnnotations(buildImports(baseService, services.flatMap(_.service.imports)))
    )

    buildType match {
      case BuildType.Api | BuildType.ApiInternal | BuildType.ApiPartner | BuildType.ApiMisc => service
      case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiMiscEvent => createEventService(service)
    }
  }

  /**
   * Filter imports to the namespaces actually referenced in the service and ensure each import namespace
   * is specified exactly once.
   */
  private[this] def buildImports(baseService: Service, imports: Seq[Import]): Seq[Import] = {
    val allNamespaces = ApiBuilderService(baseService).allTypes.map(_.namespace).toSet
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
      sys.error("Resource is missing the 'docs' attribute")
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

    dups(allPaths, "path")
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
    val names: Seq[ContextualValue] = services.flatMap { s =>
      s.models.map { m =>
        ContextualValue(
          context = s"${s.name}:${m.name}",
          value = m.name
        )
      }
    } ++ services.flatMap { s =>
      s.unions.map { u =>
        ContextualValue(
          context = s"${s.name}:${u.name}",
          value = u.name
        )
      }
    } ++ services.flatMap { s =>
      s.enums.map { e =>
        ContextualValue(
          context = s"${s.name}:${e.name}",
          value = e.name
        )
      }
    }

    dups(names, "record")
  }

  /**
    * Returns an error message if there are duplicate values
    */
  private[oneapi] def dups(values: Seq[ContextualValue], label: String): ValidatedNec[String, Unit] = {
    values.groupBy(_.value.toLowerCase).filter { _._2.size > 1 }.keys.toSeq.sorted.map { dup =>
      val dupValues = values.filter { v => dup == v.value.toLowerCase }
      assert(dupValues.size >= 2, s"Could not find duplicates for value[$dup]")
      (s"Duplicate $label[$dup] in: " + dupValues.map(_.context).sorted.mkString(", ")).invalidNec
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

  private[this] def docsAttribute(module: Module): Attribute = Attribute(
    name = "docs",
    value = Json.obj(
      "module" -> module.name
    )
  )

}
