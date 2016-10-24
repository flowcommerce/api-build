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
    service.models.filter(isError(_)).flatMap(validateModel(_))
  }

  def isError(model: Model): Boolean = {
    model.name.endsWith("_error")
  }

  def validateModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "code" :: "messages" :: rest => {
        val codeErrors = model.fields(0).`type` == "string" match {
          case true => Nil
          case false => Seq("error models require the type of the 'code' field to be 'string'")
        }

        val messagesErrors = model.fields(1).`type` == "[string]" match {
          case true => Nil
          case false => Seq("error models require the type of the 'messages' field to be '[string]'")
        }

        codeErrors ++ messagesErrors
      }

      case _ => {
        val codeErrors = fieldNames.contains("code") match {
          case false => Seq("error models require a field named 'code'")
          case true => {
            fieldNames match {
              case "code" :: rest => Nil
              case _ => Seq("error models require the the first field to be named 'code'")
            }
          }
        }

        val messagesErrors = fieldNames.contains("messages") match {
          case false => Seq("error models require a field named 'messages'")
          case true => {
            fieldNames match {
              case f :: "messages" :: rest => Nil
              case _ => Seq("error models require the the second field to be named 'messages'")
            }
          }
        }

        codeErrors ++ messagesErrors
      }
    }
  }
  
}
