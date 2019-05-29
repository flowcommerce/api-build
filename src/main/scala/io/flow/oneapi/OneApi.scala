package io.flow.oneapi

import io.apibuilder.spec.v0.models._
import io.flow.build.BuildType
import play.api.libs.json.{Json, JsString}

private[oneapi] case class ContextualValue(context: String, value: String)

case class OneApi(
  buildType: BuildType,
  flowApi: Service,
  services: Seq[Service]
) {

  private[this] val MergeResourcePathsHack = Map(
    "organization" -> "/organizations",
    "timezone" -> "/",
    "query_builder" -> "/:organization/query/builders"
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

  private[this] lazy val FlowApiImport = Import(
    uri = "https://app.apibuilder.io/flow/api/latest/service.json",
    namespace = flowApi.namespace,
    organization = Organization(key = flowApi.organization.key),
    application = Application(key = flowApi.application.key),
    version = "latest",
    enums = flowApi.enums.map(_.name),
    unions = flowApi.unions.map(_.name),
    models = flowApi.models.map(_.name),
    annotations = Nil
  )
  println(s"flowApi.namespace: ${flowApi.namespace}")
  //println(s"flowApi Models: " + flowApi.models.map(_.name).mkString("\n -  "))

  def buildOneApi(): Service = {
    def nonApiImports = services.flatMap { _.imports }.map { i =>
      if (TextDatatype.definedInService(flowApi, i.namespace)) {
        FlowApiImport
      } else {
        i
      }
    }.distinct

    val (name, key, ns, imports) = buildType match {
      case BuildType.Api => {
        val imports = services.flatMap { _.imports }.filter { i =>
          !TextDatatype.definedInService(i.namespace)
        }
        ("API", "api", "io.flow", imports)
      }

      case BuildType.ApiEvent => {
        ("API Event", "api-event", "io.flow.event", nonApiImports)
      }

      case BuildType.ApiInternal => {
        ("API Internal", "api-internal", "io.flow.internal", nonApiImports)
      }

      case BuildType.ApiInternalEvent => {
        ("API Internal Event", "api-internal-event", "io.flow.internal.event", nonApiImports)
      }

      case BuildType.ApiMisc => {
        ("API misc", "api-misc", "io.flow.misc", nonApiImports)
      }

      case BuildType.ApiMiscEvent => {
        ("API misc Event", "api-misc-event", "io.flow.misc.event", nonApiImports)
      }

      case BuildType.ApiPartner => {
        ("API Partner", "api-partner", "io.flow.partner", nonApiImports)
      }
    }

    val parser = TextDatatypeParser(flowApi, buildType)
    val localTypes: Seq[String] = services.flatMap { s =>
      s.enums.map(e => withNamespace(s, e.name)) ++ s.models.map(m => withNamespace(s, m.name)) ++ s.unions.map(u => withNamespace(s, u.name))
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
        s.enums
      }.sortBy { _.name.toLowerCase },

      models = services.flatMap { s =>
        s.models.map(localizeModel(parser, _))
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
      ).sortBy { resourceSortKey },
      annotations = allAnnotations
    )

    println(s"Service imports: ${service.imports.map(_.namespace).mkString(", ")}")
    buildType match {
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
    ApibuilderType(service, name) match {
      case None => name
      case Some(t) => t.qualified
    }
  }

  private[this] def normalizeName(parser: TextDatatypeParser, localTypes: Seq[String], service: Service, resource: Resource): Resource = {
    val qualifiedName = withNamespace(service, resource.`type`)

    val finalType = if (localTypes.contains(qualifiedName)) {
      parser.toString(parser.parse(qualifiedName))
    } else {
      qualifiedName
    }

    println(s" $qualifiedName = $finalType")
    resource.copy(
      `type` = finalType
    )
  }

  /**
    * Merges the two resources:
    *   - Takes fields from the first resource if both provide (e.g. decription)
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

  def localizeModel(parser: TextDatatypeParser, model: Model): Model = {
    model.copy(
      fields = model.fields.map(localizeField(parser, _))
    )
  }

  def localizeField(parser: TextDatatypeParser, field: Field): Field = {
    field.copy(
      `type` = localizeType(parser, field.`type`),
      description = field.description match {
        case Some(d) => Some(d)
        case None => DefaultFieldDescriptions.get(field.name)
      }
    )
  }

  def localizeUnion(parser: TextDatatypeParser, union: Union): Union = {
    union.copy(
      types = union.types.map(localizeUnionType(parser, _))
    )
  }

  def localizeUnionType(parser: TextDatatypeParser, ut: UnionType): UnionType = {
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
      10000 + Module.moduleSortIndex(moduleName),
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

  def localizeOperation(parser: TextDatatypeParser, op: Operation): Operation = {
    op.copy(
      body = op.body.map(localizeBody(parser, _)),
      parameters = op.parameters.map(localizeParameter(parser, _)),
      responses = op.responses.map(localizeResponse(parser, _))
    )
  }

  def localizeBody(parser: TextDatatypeParser, body: Body): Body = {
    body.copy(
      `type` = localizeType(parser, body.`type`)
    )
  }

  def localizeParameter(parser: TextDatatypeParser, param: Parameter): Parameter = {
    param.copy(
      `type` = localizeType(parser, param.`type`),
      description = param.description match {
        case Some(d) => Some(d)
        case None => DefaultParameterDescriptions.get(param.name)
      }
    )
  }

  def localizeResponse(parser: TextDatatypeParser, response: Response): Response = {
    response.copy(
      `type` = localizeType(parser, response.`type`),
      description = response.description match {
        case Some(d) => Some(d)
        case None => DefaultResponseDescriptions.get(responseCodeToString(response.code))
      }
    )
  }

  def responseCodeToString(code: ResponseCode): String = {
    code match {
      case ResponseCodeInt(c) => c.toString
      case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => "*"
    }
  }

  def localizeType(parser: TextDatatypeParser, name: String): String = {
    buildType match {
      case BuildType.Api => parser.toString(parser.parse(name))
      case BuildType.ApiEvent => name
      case BuildType.ApiInternal => parser.toString(parser.parse(name))
      case BuildType.ApiInternalEvent | BuildType.ApiPartner | BuildType.ApiMisc | BuildType.ApiMiscEvent => name
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

    dups(allPaths, "path")
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

    dups(names, "record")
  }

  /**
    * Returns an error message if there are duplicate values
    */
  private[oneapi]def dups(values: Seq[ContextualValue], label: String): Seq[String] = {
    values.groupBy(_.value.toLowerCase).filter { _._2.size > 1 }.keys.toSeq.sorted.map { dup =>
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
