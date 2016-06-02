package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models._

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
    enums = services.flatMap(_.enums.map(localize(_))),
    models = services.flatMap(_.models.map(localize(_))),
    unions = services.flatMap(_.unions.map(localize(_))),
    resources = Nil,
    attributes = Nil
  )

  def majorVersion(version: String): Int = {
    version.split("\\.").headOption.getOrElse {
      sys.error(s"Version[$version] must be in semver")
    }.toInt
  }

  def localize(enum: Enum): Enum = {
    enum
  }

  def localize(model: Model): Model = {
    model.copy(
      fields = model.fields.map(localize(_))
    )
  }

  def localize(field: Field): Field = {
    field.copy(
      `type` = localizeType(field.`type`)
    )
  }

  def localize(union: Union): Union = {
    union.copy(
      types = union.types.map(localize(_))
    )
  }

  def localize(ut: UnionType): UnionType = {
    ut.copy(
      `type` = localizeType(ut.`type`)
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
