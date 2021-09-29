package io.flow.oneapi

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models._
import io.flow.build.BuildType
import play.api.libs.json.{JsString, Json}

private[oneapi] case class ContextualValue(context: String, value: String)

case class OneApi(
  buildType: BuildType,
  services: Seq[Service]
) {

  private[this] val MergeResourcePathsHack = Map(
    "organization" -> "/organizations",
    "timezone" -> "/",
    "io.flow.reference.v0.models.timezone" -> "/",
    "io.flow.common.v0.models.organization" -> "/organizations",
    "io.flow.external.paypal.v1.models.webhook_event" -> "/payment/callbacks"
  )

  private[this] val DefaultFieldDescriptions = Map(
    "id" -> "Globally unique identifier",
    "number" -> "Client's unique identifier for this object",
    "organization" -> "Refers to your organization's account identifier"
  )

  private[this] val DefaultParameterDescriptions = Map(
    "id" -> "Filter by one or more IDs of this resource",
    "limit" -> "The maximum number of results to return",
    "offset" -> "The number of results to skip before returning results",
    "organization" -> "Refers to your organization's account identifier"
  )

  private[this] val DefaultResponseDescriptions = Map(
    "200" -> "Successful response",
    "201" -> "Operation succeeded and the resource was created",
    "204" -> "Operation succeeded. No content is returned",
    "401" -> "Authorization failed",
    "404" -> "Resource was not found",
    "422" -> "One or more errors were found with the data sent in the request. The body of the response contains specific details on what data failed validation."
  )

  private[this] val canonical = services.find(_.name == "common").getOrElse {
    services.headOption.getOrElse {
      sys.error("Must have at least one service")
    }
  }

  def process(): Either[Seq[String], Service] = {
    val pathErrors = validatePaths()
    val duplicateRecordErrors = validateRecordNames()

    (pathErrors ++ duplicateRecordErrors).toList match {
      case Nil => {
        Right(buildOneApi())
      }
      case errors => {
        Left(errors)
      }
    }
  }

  private[this] def buildOneApi(): Service = {
    val localTypes: Set[String] = services.flatMap { s =>
      s.enums.map(e => withNamespace(s, e.name)) ++ s.models.map(m => withNamespace(s, m.name)) ++ s.unions.map(u => withNamespace(s, u.name))
    }.toSet
    val parser = TextDatatypeParser(localTypes)

    // Annotations are not namespaced, they're global. For convenience, we'll collect them from
    // all imports and add them to the root service
    val allAnnotations = services.flatMap { _.imports.flatMap(_.annotations) }.distinctBy(_.name)

    val baseService = Service(
      apidoc = canonical.apidoc,
      name = buildType.name,
      organization = canonical.organization,
      application = Application(
        key = buildType.key
      ),
      namespace = s"${buildType.namespace}.v" + majorVersion(canonical.version),
      version = canonical.version,
      baseUrl = Some(
        canonical.baseUrl.getOrElse {
          "https://api.flow.io"
        }
      ),
      description = canonical.description,
      info = canonical.info,
      headers = Nil,
      imports = Nil,
      attributes = Nil,

      enums = services.flatMap { s =>
        s.enums
      }.sortBy { _.name.toLowerCase },

      models = services.flatMap { s =>
        s.models.map(localizeModel(parser, _))
      }.sortBy { _.name.toLowerCase },

      interfaces = services.flatMap { s =>
        s.interfaces.map(localizeInterface(parser, _))
      }.sortBy { _.name.toLowerCase },

      unions = services.flatMap { s =>
        s.unions.map(localizeUnion(parser, _))
      }.sortBy { _.name.toLowerCase },

      resources = mergeResources(
        services.flatMap { s =>
          s.resources.
            map(normalizeName(parser, localTypes, s, _)).
            map(localize(parser, s, _))
        }
      ) match {
        case Invalid(errors) => sys.error(errors.toList.mkString("\n"))
        case Valid(r) => r.toList.sortBy { resourceSortKey }
      },
      annotations = allAnnotations
    )

    val service = baseService.copy(
      imports = stripAnnotations(buildImports(baseService, services))
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
  private[this] def buildImports(baseService: Service, services: Seq[Service]): Seq[Import] = {
    val parser = TextDatatypeParser(Set.empty)
    val allNames = (
      baseService.models.map(_.name) ++ baseService.unions.map(_.name) ++ baseService.enums.map(_.name) ++ baseService.interfaces.map(_.name)
    )
    val allNamespaces = allNames.flatMap(parser.toNamespace).toSet
    val availableImports = services.flatMap(_.imports).distinctBy(_.namespace)
    println("all names: " + allNames.sorted.mkString(", "))
    println("Needed imports: " + allNamespaces.toList.sorted.mkString(", "))
    println("available imports: " + availableImports.map(_.namespace).sorted.mkString(", "))
    availableImports.filter { imp =>
      allNamespaces.contains(imp.namespace)
    }
  }

  private[this] def stripAnnotations(imports: Seq[Import]): Seq[Import] = {
    imports.map(_.copy(annotations = Nil))
  }

  @scala.annotation.tailrec
  private[this] def mergeResources(resources: Seq[Resource], merged: Seq[Resource] = Nil): ValidatedNec[String, Seq[Resource]] = {
    resources.toList match {
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


  private[this] def withNamespace(service: Service, name: String): String = {
    apiBuilderType(service, name) match {
      case None => name
      case Some(apiBuilderType) => s"${service.namespace}.$apiBuilderType.$name"
    }
  }

  private[this] def normalizeName(parser: TextDatatypeParser, localTypes: Set[String], service: Service, resource: Resource): Resource = {
    val qualifiedName = withNamespace(service, resource.`type`)

    val finalType = if (localTypes.contains(qualifiedName)) {
      parser.toString(parser.parse(qualifiedName))
    } else {
      qualifiedName
    }

    resource.copy(
      `type` = finalType
    )
  }

  private[this] def apiBuilderType(service: Service, name: String): Option[String] = {
    service.enums.find(_.name == name) match {
      case Some(_) => Some("enums")
      case None => service.models.find(_.name == name) match {
        case Some(_) => Some("models")
        case None => service.unions.find(_.name == name) match {
          case None => None
          case Some(_) => Some("unions")
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

  private[this] def localizeModel(parser: TextDatatypeParser, model: Model): Model = {
    model.copy(
      fields = model.fields.map(localizeField(parser, _))
    )
  }

  private[this] def localizeInterface(parser: TextDatatypeParser, interface: Interface): Interface = {
    interface.copy(
      fields = interface.fields.map(localizeField(parser, _))
    )
  }

  private[this] def localizeField(parser: TextDatatypeParser, field: Field): Field = {
    field.copy(
      `type` = localizeType(parser, field.`type`),
      description = field.description match {
        case Some(d) => Some(d)
        case None => DefaultFieldDescriptions.get(field.name)
      }
    )
  }

  private[this] def localizeUnion(parser: TextDatatypeParser, union: Union): Union = {
    union.copy(
      types = union.types.map(localizeUnionType(parser, _))
    )
  }

  private[this] def localizeUnionType(parser: TextDatatypeParser, ut: UnionType): UnionType = {
    ut.copy(
      `type` = localizeType(parser, ut.`type`)
    )
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

  private[this] def localize(parser: TextDatatypeParser, service: Service, resource: Resource): Resource = {
    val additionalAttributes = resource.attributes.find(_.name == "docs") match {
      case None => {
        val module = Module.findByServiceName(service.name.toLowerCase).getOrElse {
          Module.General
        }
        Seq(docsAttribute(module))
      }
      case Some(_) => Nil
    }

    resource.copy(
      `type` = localizeType(parser, resource.`type`),
      operations = resource.operations.
        map { localizeOperation(parser, _) }.
        sortBy(OperationSort.key),
      description = resource.description match {
        case Some(d) => Some(d)
        case None => recordDescription(service, resource.`type`)
      },
      attributes = resource.attributes ++ additionalAttributes
    )
  }

  /**
    * If this type refers to a valid enum, model, or union, returns
    * the description associated with that record (if there is
    * one). Initial use case was to populate the resource description,
    * when empty, with the description from the model associated with
    * the resource.
    */
  private[this] def recordDescription(service: Service, typ: String): Option[String] = {
    service.enums.find(_.name == typ) match {
      case Some(e) => e.description
      case None => {
        service.models.find(_.name == typ) match {
          case Some(m) => m.description
          case None => {
            service.unions.find(_.name == typ) match {
              case Some(u) => u.description
              case None => None
            }
          }
        }
      }
    }
  }

  private[this] def localizeOperation(parser: TextDatatypeParser, op: Operation): Operation = {
    op.copy(
      body = op.body.map(localizeBody(parser, _)),
      parameters = op.parameters.map(localizeParameter(parser, _)),
      responses = op.responses.map(localizeResponse(parser, _))
    )
  }

  private[this] def localizeBody(parser: TextDatatypeParser, body: Body): Body = {
    body.copy(
      `type` = localizeType(parser, body.`type`)
    )
  }

  private[this] def localizeParameter(parser: TextDatatypeParser, param: Parameter): Parameter = {
    param.copy(
      `type` = localizeType(parser, param.`type`),
      description = param.description match {
        case Some(d) => Some(d)
        case None => DefaultParameterDescriptions.get(param.name)
      }
    )
  }

  private[this] def localizeResponse(parser: TextDatatypeParser, response: Response): Response = {
    response.copy(
      `type` = localizeType(parser, response.`type`),
      description = response.description match {
        case Some(d) => Some(d)
        case None => DefaultResponseDescriptions.get(responseCodeToString(response.code))
      }
    )
  }

  private[this] def responseCodeToString(code: ResponseCode): String = {
    code match {
      case ResponseCodeInt(c) => c.toString
      case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => "*"
    }
  }

  private[this] def localizeType(parser: TextDatatypeParser, name: String): String = {
    buildType match {
      case BuildType.Api => parser.toString(parser.parse(name))
      case BuildType.ApiEvent => toApiImport(parser, name)
      case BuildType.ApiInternal | BuildType.ApiInternalEvent | BuildType.ApiPartner | BuildType.ApiMisc | BuildType.ApiMiscEvent => name
    }
  }

  private[this] val ApiServiceRx = """^io\.flow\..+\.v0\.(\w+).(\w+)$""".r
  private[this] val ApiServiceArrayRx = """^\[io\.flow\..+\.v0\.(\w+).(\w+)\]$""".r
  private[this] val ApiServiceMapRx = """^map\[io\.flow\..+\.v0\.(\w+).(\w+)\]$""".r

  /**
    * Rewrite imports in api-event to allow imports from api
    */
  private[this] def toApiImport(parser: TextDatatypeParser, name: String): String = {
    val simpleName = parser.toString(parser.parse(name))
    if (simpleName == name) {
      name
    } else {
      name match {
        case ApiServiceRx(_, _) | ApiServiceArrayRx(_, _) | ApiServiceMapRx(_, _) => {
          // TODO: figure out how to import. we have a circular
          // dependency as api project is built after
          // api-event. Probably need to move the generate event
          // union/enum into api-event
          //
          // s"io.flow.api.v0.$apiBuilderType.$typeName"
          name
        }
        case _ => {
          sys.error(s"Failed to map import to API project for type[$name]")
        }
      }
    }
  }

  private[this] def validatePaths(): Seq[String] = {
    val allPaths: Seq[ContextualValue] = services.flatMap { s =>
      s.resources.flatMap { r =>
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

  private[this] def validateRecordNames(): Seq[String] = {
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
  private[oneapi] def dups(values: Seq[ContextualValue], label: String): Seq[String] = {
    values.groupBy(_.value.toLowerCase).filter { _._2.size > 1 }.keys.toSeq.sorted.map { dup =>
      val dupValues = values.filter { v => dup == v.value.toLowerCase }
      assert(
        dupValues.size >= 2,
        s"Could not find duplicates for value[$dup]"
      )
      s"Duplicate $label[$dup] in: " + dupValues.map(_.context).sorted.mkString(", ")
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
