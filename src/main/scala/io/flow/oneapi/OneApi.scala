package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models._
import play.api.libs.json.{Json, JsString}

private[oneapi] case class ContextualValue(context: String, value: String)

case class OneApi(services: Seq[Service]) {

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
    "401" -> "Authorization failed.",
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

  def buildOneApi() = Service(
    apidoc = canonical.apidoc,
    name = "API",
    organization = canonical.organization,
    application = Application(
      key = "api"
    ),
    namespace = "io.flow.v" + majorVersion(canonical.version),
    version = canonical.version,
    baseUrl = canonical.baseUrl,
    description = canonical.description,
    info = canonical.info,
    headers = Nil,
    imports = Nil,
    attributes = Nil,

    enums = services.flatMap { s =>
      s.enums.map(localize(s, _))
    }.sortBy { _.name.toLowerCase },

    models = services.flatMap { s =>
      s.models.map(localize(s, _))
    }.sortBy { _.name.toLowerCase },

    unions = services.flatMap { s =>
      s.unions.map(localize(s, _))
    }.sortBy { _.name.toLowerCase },

    resources = services.flatMap { s =>
      s.resources.map(localize(s, _))
    }.sortBy { resourceSortKey(_) }
  )

  /**
    * Given a string version number in semver, e.g. 1.2.3, returns the
    * major version number as an integer (e.g. 1)
    */
  def majorVersion(version: String): Int = {
    version.split("\\.").headOption.getOrElse {
      sys.error(s"Version[$version] must be in semver")
    }.toInt
  }

  def localize(service: Service, enum: Enum): Enum = {
    enum
  }

  def localize(service: Service, model: Model): Model = {
    model.copy(
      fields = model.fields.map(localize(service, _))
    )
  }

  def localize(service: Service, field: Field): Field = {
    field.copy(
      `type` = localizeType(field.`type`),
      description = (
        field.description match {
          case Some(d) => Some(d)
          case None => DefaultFieldDescriptions.get(field.name)
        }
      )
    )
  }

  def localize(service: Service, union: Union): Union = {
    union.copy(
      types = union.types.map(localize(service, _))
    )
  }

  def localize(service: Service, ut: UnionType): UnionType = {
    ut.copy(
      `type` = localizeType(ut.`type`)
    )
  }

  def resourceSortKey(resource: Resource) = {
    val docs = resource.attributes.find(_.name == "docs").getOrElse {
      sys.error("Resource is missing the 'docs' attribute")
    }
    val moduleName = (docs.value \ "module").as[JsString].value

    Seq(
      (10000 + Module.moduleSortIndex(moduleName)),
      resource.`type`.toLowerCase
    ).mkString(":")
  }

  def localize(service: Service, resource: Resource): Resource = {
    val additionalAttributes = resource.attributes.find(_.name == "docs") match {
      case None => {
        val module = Module.findByServiceName(service.name.toLowerCase).getOrElse {
          println(s"** WARNING ** Service[${service.name}] is not mapped to a module. Using ${Module.General.name}")
          Module.General
        }
        Seq(
          Attribute(
            name = "docs",
            value = Json.obj(
              "module" -> module.name
            )
          )
        )
      }
      case Some(_) => Nil
    }

    resource.copy(
      `type` = localizeType(resource.`type`),
      operations = resource.operations.map { localize(service, _) }.sortBy { op => (op.path.toLowerCase, methodSortOrder(op.method)) },
      description = (
        resource.description match {
          case Some(d) => Some(d)
          case None => recordDescription(service, resource.`type`)
        }
      ),
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

  /**
    * Returns a numeric index by which we can sort methods. This
    * allows us to present, for example, all operations with a GET
    * Method first.
    */
  def methodSortOrder(method: Method): Int = {
    method match {
      case Method.Get => 1
      case Method.Post => 2
      case Method.Put => 3
      case Method.Patch => 4
      case Method.Delete => 5
      case Method.Connect => 6
      case Method.Head => 7
      case Method.Options => 8
      case Method.Trace => 9
      case Method.UNDEFINED(_) => 10
    }
  }

  def localize(service: Service, op: Operation): Operation = {
    op.copy(
      body = op.body.map(localize(service, _)),
      parameters = op.parameters.map(localize(service, _)),
      responses = op.responses.map(localize(service, _))
    )
  }

  def localize(service: Service, body: Body): Body = {
    body.copy(
      `type` = localizeType(body.`type`)
    )
  }

  def localize(service: Service, param: Parameter): Parameter = {
    param.copy(
      `type` = localizeType(param.`type`),
      description = (
        param.description match {
          case Some(d) => Some(d)
          case None => DefaultParameterDescriptions.get(param.name)
        }
      )
    )
  }

  def localize(service: Service, response: Response): Response = {
    response.copy(
      `type` = localizeType(response.`type`),
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
      case ResponseCodeInt(code) => code.toString
      case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => "*"
    }
  }

  def localizeType(name: String): String = {
    TextDatatype.toString(TextDatatype.parse(name))
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
    val pathParts: Seq[String] = path.split("/").map { name =>
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
  def dups(values: Seq[ContextualValue], label: String): Seq[String] = {
    values.groupBy(_.value.toLowerCase).filter { _._2.size > 1 }.keys.toSeq.sorted.map { dup =>
      val dupValues = values.filter { v => dup == v.value.toLowerCase }.toSeq
      assert(dupValues.size >= 2, s"Could not find duplicates for value[$dup]")
      s"Duplicate $label[$dup] in: " + dupValues.map(_.context).sorted.mkString(", ")
    }
  }
}
