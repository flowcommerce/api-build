package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Service, Union}

/** For error models (those with an error_id field in position 1), validate:
  *
  *   a. second field is timestamp b. if 'organization', next c. if 'number', next
  */
case object ErrorModelsV1 extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    val unionsThatEndInError =
      service.unions.filter { u => !ignored(u.attributes, "error") }.filter { u => isError(u.name) }

    val modelErrors = service.models
      .filter { m => isError(m.name) }
      .filter { m => !ignored(m.attributes, "error") }
      .filter { m =>
        !unions(service, m).exists { u => isError(u.name) }
      }
      .filter { m => errorVersion(m.attributes).contains(1) }
      .flatMap(validateModel(service, _))

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

  private[this] def validateModel(service: Service, model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name).toList
    fieldNames match {
      case "code" :: "messages" :: _ => {
        val codeErrors = if (model.fields.head.`type` == "string") {
          Nil
        } else if (hasEnum(service, model.fields.head.`type`)) {
          Nil
        } else {
          Seq(error(model, model.fields.head, s"type[${model.fields.head.`type`}] must refer to a valid enum"))
        }

        val messagesErrors = validateMessageField(model, model.fields(1))

        codeErrors ++ messagesErrors
      }

      case _ => {
        val codeErrors = if (fieldNames.contains("code")) {
          fieldNames match {
            case "code" :: _ => Nil
            case _ => Seq(error(model, "first field must be 'code'"))
          }
        } else {
          Seq(error(model, "requires a field named 'code'"))
        }

        val messagesErrors = if (fieldNames.contains("messages")) {
          fieldNames match {
            case _ :: "messages" :: _ => Nil
            case _ => {
              if (fieldNames.contains("code")) {
                Seq(error(model, "second field must be 'messages'"))
              } else {
                Nil
              }
            }
          }
        } else {
          Seq(error(model, "requires a field named 'messages'"))
        }

        codeErrors ++ messagesErrors
      }
    }
  }

  def validateMessageField(model: Model, field: Field): Seq[String] = {
    val typeErrors = if (field.`type` == "[string]") {
      Nil
    } else {
      Seq(error(model, field, "type must be '[string]'"))
    }

    val minimumErrors = field.minimum match {
      case Some(n) => {
        if (n >= 1) {
          Nil
        } else {
          Seq(error(model, field, "minimum must be >= 1"))
        }
      }
      case None => {
        Seq(error(model, field, "missing minimum"))
      }
    }

    typeErrors ++ minimumErrors
  }

}
