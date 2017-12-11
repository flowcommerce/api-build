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

  private[this] def validateModel(service: Service, model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "code" :: "messages" :: rest => {
        val codeErrors = model.fields(0).`type` == "string" match {
          case true => Nil
          case false => {
            hasEnum(service, model.fields.head.`type`) match {
              case false => Seq(error(model, model.fields.head, "type must be 'string' or a valid enum"))
              case true => Nil
            }
          }
        }

        val messagesErrors = validateMessageField(model, model.fields(1))

        codeErrors ++ messagesErrors
      }

      case _ => {
        val codeErrors = fieldNames.contains("code") match {
          case false => Seq(error(model, "requires a field named 'code'"))
          case true => {
            fieldNames match {
              case "code" :: rest => Nil
              case _ => Seq(error(model, "first field must be 'code'"))
            }
          }
        }

        val messagesErrors = fieldNames.contains("messages") match {
          case false => Seq(error(model, "requires a field named 'messages'"))
          case true => {
            fieldNames match {
              case f :: "messages" :: rest => Nil
              case _ => {
                fieldNames.contains("code") match {
                  case false => Nil
                  case true => Seq(error(model, "second field must be 'messages'"))
                }
              }
            }
          }
        }

        codeErrors ++ messagesErrors
      }
    }
  }

  def validateMessageField(model: Model, field: Field): Seq[String] = {
    val typeErrors = field.`type` == "[string]" match {
      case true => Nil
      case false => Seq(error(model, field, "type must be '[string]'"))
    }

    val minimumErrors = field.minimum match {
      case Some(n) => {
        n >= 1 match {
          case true => Nil
          case false => Seq(error(model, field, "minimum must be >= 1"))
        }
      }
      case None => {
        Seq(error(model, field, "missing minimum"))
      }
    }

    typeErrors ++ minimumErrors
  }

}
