package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Field, Model, Service}

/**
  * For error models (those with an error_id field in position 1), validate:
  * 
  *   a. second field is timestamp
  *   b. if 'organization', next
  *   c. if 'number', next
  */
case object ErrorModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.filter(isError(_)).flatMap(validateModel(service, _))
  }

  def isError(model: Model): Boolean = {
    model.name.endsWith("_error")
  }

  private[this] def validateModel(service: Service, model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "code" :: "messages" :: rest => {
        val codeErrors = model.fields(0).`type` == "string" match {
          case true => Nil
          case false => {
            service.enums.find(_.name == model.fields(0).`type`) match {
              case None => Seq(error(model, model.fields(0), "type must be 'string' or a valid enum"))
              case Some(_) => Nil
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
