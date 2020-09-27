package io.flow.lint.util

import io.apibuilder.spec.v0.models.{Field, Model, Service}
import io.flow.lint.linters.Helpers

/**
 * Validate model contains fields named 'code', 'messages'
 */
case object ErrorModelsV1 extends Helpers {
  val Version = "1"

  def validateModel(service: Service, model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name).toList
    fieldNames match {
      case "code" :: "messages" :: _ => {
        val codeErrors = model.fields.head.`type` == "string" match {
          case true => Nil
          case false => {
            if (hasEnum(service, model.fields.head.`type`)) {
              Nil
            } else {
              Seq(error(model, model.fields.head, "type must be 'string' or a valid enum"))
            }
          }
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
