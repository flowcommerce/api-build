package io.flow.oneapi

import io.apibuilder.spec.v0.models._
import io.flow.build.{BuildType, Config}
import play.api.libs.json.{Json, JsString}

private[oneapi] case class ContextualValue(context: String, value: String)

case class OneApi(
  config: Config,
  services: Seq[Service]
) {

  private[this] val MergeResourcePathsHack = Map(
    "organization" -> "/organizations",
    "timezone" -> "/",
    "query_builder" -> "/:organization/query/builders",
    "country" -> "/reference/countries"
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

  private[this] val dups = findDups {
    services.flatMap { s =>
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
  }

  def process(): Either[Seq[String], Service] = {
    val pathErrors = validatePaths()
    val duplicateRecordErrors = if (config.dedup) Seq() else validateRecordNames()

    (pathErrors ++ duplicateRecordErrors).toList match {
      case Nil => {
        Right(buildOneApi())
      }
      case errors => {
        Left(errors)
      }
    }
  }

  def buildOneApi(): Service = {
    val (name, key, ns, imports) = config.buildType match {
      case BuildType.Api => {
        ("API", "api", "io.flow", Nil)
      }

      case BuildType.ApiEvent => {
        val imports = services.flatMap { _.imports }
        ("API Event", "api-event", "io.flow.event", imports)
      }

      case BuildType.ApiInternal => {
        val imports = services.flatMap { _.imports }
        ("API Internal", "api-internal", "io.flow.internal", imports)
      }

      case BuildType.ApiInternalEvent => {
        val imports = services.flatMap { _.imports }
        ("API Internal Event", "api-internal-event", "io.flow.internal.event", imports)
      }

      case BuildType.ApiMisc => {
        val imports = services.flatMap { _.imports }
        ("API misc", "api-misc", "io.flow.misc", imports)
      }

      case BuildType.ApiMiscEvent => {
        val imports = services.flatMap { _.imports }
        ("API misc Event", "api-misc-event", "io.flow.misc.event", imports)
      }

      case BuildType.ApiPartner => {
        val imports = services.flatMap { _.imports }
        ("API Partner", "api-partner", "io.flow.partner", imports)
      }
    }

    val parser = TextDatatypeParser()
    val localTypes: Seq[String] = services.flatMap { s =>
      s.enums.map(e => withNamespace(s, e.name)) ++
        s.models.map(m => withNamespace(s, m.name)) ++
        s.unions.map(u => withNamespace(s, u.name))
    }

    //Annotations are not namespaced, they're global. For convenience, we'll collect them from all imports and add them
    //to the root service
    val allAnnotations = services.flatMap { _.imports.flatMap(_.annotations) }.distinct
    val importsWithNoAnnotations = imports.map(_.copy(annotations = Nil))

    val service = Service(
      apidoc = canonical.apidoc,
      name = name,
      organization = canonical.organization,
      application = Application(
        key = key
      ),
      namespace = s"${ns}.v" + majorVersion(canonical.version),
      version = canonical.version,
      baseUrl = Some(
        canonical.baseUrl.getOrElse {
          "https://api.flow.io"
        }
      ),
      description = canonical.description,
      info = canonical.info,
      headers = Nil,
      imports = importsWithNoAnnotations,
      attributes = Nil,

      enums = services.flatMap { s =>
        s.enums.map(localize(parser, s, _))
      }.sortBy { _.name.toLowerCase },

      models = services.flatMap { s =>
        s.models.map(localize(parser, s, _))
      }.sortBy { _.name.toLowerCase },

      unions = services.flatMap { s =>
        s.unions.map(localize(parser, s, _))
      }.sortBy { _.name.toLowerCase },
     
      resources = mergeResources(
        services.flatMap { s =>
          s.resources.
            map(normalizeName(parser, localTypes, s, _)).
            map(localize(parser, s, _))
        }
      ).sortBy { resourceSortKey },
      annotations = allAnnotations
    )

    config.buildType match {
      case BuildType.Api | BuildType.ApiInternal | BuildType.ApiPartner | BuildType.ApiMisc => service
      case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiMiscEvent => createEventService(service)
    }
  }

  @scala.annotation.tailrec
  private[this] def mergeResources(resources: Seq[Resource], merged: Seq[Resource] = Nil): Seq[Resource] = {
    resources.toList match {
      case Nil => merged
      case one :: rest => {
        merged.find(_.`type` == one.`type`) match {
          case None => mergeResources(rest, merged ++ Seq(one))
          case Some(r) => mergeResources(rest, merged.filter(_.`type` != one.`type`) ++ Seq(merge(r, one)))
        }
      }
    }
  }


  private[this] def withNamespace(service: Service, name: String): String = {
    apidocType(service, name) match {
      case None => name
      case Some(apidocType) => s"${service.namespace}.$apidocType.$name"
    }
  }

  private[this] def normalizeName(parser: TextDatatypeParser, localTypes: Seq[String], service: Service, resource: Resource): Resource = {
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

  private[this] def apidocType(service: Service, name: String): Option[String] = {
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
  private[this] def merge(a: Resource, b: Resource): Resource = {
    assert(a.`type` == b.`type`)

    // Ideally the resource paths are the same; if not we have to
    // explicitly choose the resource path we want. This will
    // influence method names in the code generators and thus
    // important to get right.
    val allPaths = Seq(a.path, b.path).flatten.distinct
    val path: Option[String] = allPaths.toList match {
      case Nil => None
      case one :: Nil => Some(one)
      case multiple => {
        MergeResourcePathsHack.get(a.`type`) match {
          case Some(defaultPath) => {
            if (allPaths.contains(defaultPath)) {
              Some(defaultPath)
            } else if (defaultPath == "/") {
              Some(defaultPath)
            } else {
              sys.error(s"Error while merging resources of type[${a.`type`}]: Default path $defaultPath is not specified on either resource[${allPaths}]")
            }
          }
          case None => {
            val (internal, public) = multiple.partition(p => p.startsWith("/internal"))
            public match {
              case Nil => internal.headOption
              case one :: Nil => Some(one)
              case multiplePublic => {
                sys.error(s"Cannot merge resources of type[${a.`type`}] with more than one non internal path:\n" +
                  multiplePublic.sorted.mkString("  - ", "\n  - ", "\n") +
                  "To resolve - edit api-build:src/main/scala/io/flow/oneapi/OneApi.scala and update MergeResourcePathsHack to add an explicit path for this resource"
                )
              }
            }
          }
        }
      }
    }

    val attributeNames = a.attributes.map(_.name)

    a.copy(
      path = path,
      description = a.description match {
        case Some(desc) => Some(desc)
        case None => b.description
      },
      operations = a.operations ++ b.operations,
      attributes = a.attributes ++ b.attributes.filter(attr => !attributeNames.contains(attr.name))
    )
  }

  /**
    * Given a string version number in semver, e.g. 1.2.3, returns the
    * major version number as an integer (e.g. 1)
    */
  def majorVersion(version: String): Int = {
    version.split("\\.").headOption.getOrElse {
      sys.error(s"Version[$version] must be in semver")
    }.toInt
  }

  private def namespaceTypeName(ns: String, t: String) =
    (ns.split('.') ++ t.split('_')).mkString("_")

  def localize(parser: TextDatatypeParser, service: Service, enum: Enum): Enum = {
    if (dups.contains(enum.name))
      enum.copy(name = namespaceTypeName(service.namespace, enum.name))
    else
      enum
  }

  def localize(parser: TextDatatypeParser, service: Service, model: Model): Model = {
    model.copy(
      name =
        if (dups.contains(model.name))
          namespaceTypeName(service.namespace, model.name)
        else
          model.name,
      fields = model.fields.map(localize(parser, service, _))
    )
  }

  def localize(parser: TextDatatypeParser, service: Service, field: Field): Field = {
    field.copy(
      `type` = localizeType(parser, field.`type`),
      description = (
        field.description match {
          case Some(d) => Some(d)
          case None => DefaultFieldDescriptions.get(field.name)
        }
      )
    )
  }

  def localize(parser: TextDatatypeParser, service: Service, union: Union): Union = {
    union.copy(
      name =
        if (dups.contains(union.name))
          namespaceTypeName(service.namespace, union.name)
        else
          union.name,
      types = union.types.map(localize(parser, service, _))
    )
  }

  def localize(parser: TextDatatypeParser, service: Service, ut: UnionType): UnionType = {
    ut.copy(
      `type` = localizeType(parser, ut.`type`)
    )
  }

  def resourceSortKey(resource: Resource): String = {
    val docs = resource.attributes.find(_.name == "docs").getOrElse {
      sys.error("Resource is missing the 'docs' attribute")
    }
    val moduleName = (docs.value \ "module").as[JsString].value

    Seq(
      (10000 + Module.moduleSortIndex(moduleName)),
      resource.`type`.toLowerCase
    ).mkString(":")
  }

  def localize(parser: TextDatatypeParser, service: Service, resource: Resource): Resource = {
    val additionalAttributes = resource.attributes.find(_.name == "docs") match {
      case None => {
        val module = Module.findByServiceName(service.name.toLowerCase).getOrElse {
          println(s"** WARNING ** Service[${service.name}] is not mapped to a module. Using ${Module.General.name}")
          Module.General
        }
        Seq(docsAttribute(module))
      }
      case Some(_) => Nil
    }

    resource.copy(
      `type` = localizeType(parser, resource.`type`),
      operations = resource.operations.
        map { localize(parser, service, _) }.
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
  def recordDescription(service: Service, typ: String): Option[String] = {
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

  def localize(parser: TextDatatypeParser, service: Service, op: Operation): Operation = {
    op.copy(
      body = op.body.map(localize(parser, service, _)),
      parameters = op.parameters.map(localize(parser, service, _)),
      responses = op.responses.map(localize(parser, service, _))
    )
  }

  def localize(parser: TextDatatypeParser, service: Service, body: Body): Body = {
    body.copy(
      `type` = localizeType(parser, body.`type`)
    )
  }

  def localize(parser: TextDatatypeParser, service: Service, param: Parameter): Parameter = {
    param.copy(
      `type` = localizeType(parser, param.`type`),
      description = (
        param.description match {
          case Some(d) => Some(d)
          case None => DefaultParameterDescriptions.get(param.name)
        }
      )
    )
  }

  def localize(parser: TextDatatypeParser, service: Service, response: Response): Response = {
    response.copy(
      `type` = localizeType(parser, response.`type`),
      description = (
        response.description match {
          case Some(d) => Some(d)
          case None => DefaultResponseDescriptions.get(responseCodeToString(response.code))
        }
      )
    )
  }

  def responseCodeToString(code: ResponseCode): String = {
    code match {
      case ResponseCodeInt(c) => c.toString
      case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => "*"
    }
  }

  def localizeType(parser: TextDatatypeParser, name: String): String = {
    config.buildType match {
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
  def toApiImport(parser: TextDatatypeParser, name: String): String = {
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
          // s"io.flow.api.v0.$apidocType.$typeName"
          name
        }
        case _ => {
          sys.error(s"Failed to map import to API project for type[$name]")
        }
      }
    }
  }

  def validatePaths(): Seq[String] = {
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

    checkDups(allPaths, "path")
  }

  def normalizePath(method: Method, path: String): String = {
    val pathParts: Seq[String] = path.split("[/.]").map { name =>
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

  def validateRecordNames(): Seq[String] = {
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

    checkDups(names, "record")
  }

  def findDups(values: Seq[ContextualValue]): Seq[String] =
    values.groupBy(_.value.toLowerCase).filter { _._2.size > 1 }.keys.toSeq.sorted

  /**
    * Returns an error message if there are duplicate values
    */
  def checkDups(values: Seq[ContextualValue], label: String): Seq[String] = {
    findDups(values).map { dup =>
      val dupValues = values.filter { v => dup == v.value.toLowerCase }
      assert(
        dupValues.size >= 2,
        s"Could not find duplicates for value[$dup]"
      )
      s"Duplicate $label[$dup] in: " + dupValues.map(_.context).sorted.mkString(", ")
    }
  }

  def createEventService(service: Service): Service = {
    val event = createEventUnion(service.unions)
    val eventType = createEventTypeEnum(event)
    service.copy(
      enums = service.enums ++ Seq(eventType),
      unions = Seq(event)
    )
  }

  def createEventUnion(unions: Seq[Union]): Union = {
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

  def createEventTypeEnum(event: Union): Enum = {
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

  def docsAttribute(module: Module) = Attribute(
    name = "docs",
    value = Json.obj(
      "module" -> module.name
    )
  )

}
