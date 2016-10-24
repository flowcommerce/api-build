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

  def isPartOfUnion(service: Service, model: Model): Boolean = {
    service.unions.find { u =>
      u.types.map(_.`type`).contains(model.name)
    }.isDefined
  }

  def validateModel(service: Service, model: Model): Seq[String] = {
    isPartOfUnion(service, model) match {
      case true => validateUnionModel(model)
      case false => validateStandaloneModel(model)
    }
  }

  private[this] def validateUnionModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "messages" :: rest => {
        val codeErrors = fieldNames.contains("code") match {
          case true => Seq(error(model, "field named 'code' must come from union type discriminator"))
          case false => Nil
        }

        val messagesErrors = model.fields(0).`type` == "[string]" match {
          case true => Nil
          case false => Seq(error(model, model.fields(0), "type must be '[string]'"))
        }

        codeErrors ++ messagesErrors
      }

      case _ => {
        fieldNames.contains("messages") match {
          case false => Seq(error(model, "requires a field named 'messages'"))
          case true => {
            fieldNames match {
              case "messages" :: rest => Nil
              case _ => Seq(error(model, "first field must be 'messages'"))
            }
          }
        }
      }
    }
  }

  private[this] def validateStandaloneModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "code" :: "messages" :: rest => {
        val codeErrors = model.fields(0).`type` == "string" match {
          case true => Nil
          case false => Seq(error(model, model.fields(0), "type must be 'string'"))
        }

        val messagesErrors = model.fields(1).`type` == "[string]" match {
          case true => Nil
          case false => Seq(error(model, model.fields(1), "type must be '[string]'"))
        }

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
  
}
