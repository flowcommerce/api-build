package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Service, Union}

/**
  * For error models (those with an error_id field in position 1), validate:
  *
  *   a. second field is timestamp
  *   b. if 'organization', next
  *   c. if 'number', next
  */
case object ErrorModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    val unionsThatEndInError = service.unions.filter { u => isError(u.name) }

    val modelErrors = service.models.
      filter { m => isError(m.name) }.
      filter { m =>
        !unions(service, m).exists { u => isError(u.name) }
      }.
      flatMap(validateModel(service, _))

    val unionErrors = unionsThatEndInError.flatMap(validateUnion(service, _))

    modelErrors ++ unionErrors
  }

  private[this] def validateUnion(service: Service, union: Union): Seq[String] = {
    val discriminatorFields: Seq[Field] = union.discriminator.map { discName =>
      Field(
        name = discName,
        `type` = "string",
        required = true
      )
    }.toSeq

    union.types.flatMap { t =>
      service.models.find(_.name == t.`type`) match {
        case None => {
          Seq(error(union, t, "Type must refer to a model to be part of an 'error' union type"))
        }

        case Some(m) => {
          val nameErrors = if (isError(m.name)) {
            Nil
          } else {
            Seq(error(union, t, "Model name must end with '_error'"))
          }

          val modelErrors = validateModel(
            service,
            m.copy(
              fields = discriminatorFields ++ m.fields
            )
          )

          nameErrors ++ modelErrors
        }
      }
    }

  }

  private[this] val DefaultVersion = "1"
  private[this] def validateModel(service: Service, model: Model): Seq[String] = {
    println(s"linterAttributeValues(model.attributes, error_version): ${linterAttributeValues(model.attributes, "error_version")}")
    linterAttributeValues(model.attributes, "error_version").toList match {
      case Nil => validateModelVersion(service, model, DefaultVersion)
      case one :: Nil => validateModelVersion(service, model, one)
      case _ => attributeError(model, "multiple values found for attribute")
    }
  }

  private[this] def validateModelVersion(service: Service, model: Model, version: String): Seq[String] = {
    println(s"VERSI: ${version}")
    version.trim match {
      case DefaultVersion => ErrorModelsV1.validateModel(service, model)
      case "2" => ErrorModelsV2.validateModel(model)
      case other => attributeError(model, s"invalid value '$other' - must be '1' or '2'")
    }
  }

  private[this] def attributeError(model: Model, message: String): Seq[String] = {
    Seq(error(model, s"attribute 'linter' name 'error_version' $message"))
  }
}

