package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models._
import play.api.libs.json.{Json, JsString}

case class ContextualValue(context: String, value: String)

case class OneApi(services: Seq[Service]) {

  private[this] val common = services.find(_.name == "common").getOrElse {
    sys.error("Must have a service named common")
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
    apidoc = common.apidoc,
    name = "Flow Commerce API",
    organization = common.organization,
    application = Application(
      key = "api"
    ),
    namespace = "io.flow.v" + majorVersion(common.version),
    version = common.version,
    baseUrl = common.baseUrl,
    description = common.description,
    info = common.info,
    headers = Nil,
    imports = Nil,
    attributes = Nil,

    enums = services.flatMap { s =>
      s.enums.map(localize(s, _))
    }.sortWith { _.name.toLowerCase < _.name.toLowerCase },

    models = services.flatMap { s =>
      s.models.map(localize(s, _))
    }.sortWith { _.name.toLowerCase < _.name.toLowerCase },

    unions = services.flatMap { s =>
      s.unions.map(localize(s, _))
    }.sortWith { _.name.toLowerCase < _.name.toLowerCase },

    resources = services.flatMap { s =>
      s.resources.map(localize(s, _))
    }.sortWith { resourceSortKey(_) < resourceSortKey(_) }
  )

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
      `type` = localizeType(field.`type`)
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
    val module = Module.findByServiceName(service.name.toLowerCase).getOrElse {
      println(s"** WARNING ** Service[${service.name}] is not mapped to a module. Using ${Module.General.name}")
      Module.General
    }

    resource.copy(
      `type` = localizeType(resource.`type`),
      operations = resource.operations.map { localize(service, _) }.sortBy { op => (op.path.toLowerCase, methodSortOrder(op.method)) },
      attributes = resource.attributes ++ Seq(
        Attribute(
          name = "docs",
          value = Json.obj(
            "module" -> module.name
          )
        )
      )
    )
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
      `type` = localizeType(param.`type`)
    )
  }

  def localize(service: Service, response: Response): Response = {
    response.copy(
      `type` = localizeType(response.`type`)
    )
  }
  
  def localizeType(name: String): String = {
    name.split("\\.").last
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
