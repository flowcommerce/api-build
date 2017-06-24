package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Model, Service}

/**
  * For event models (those with an event_id field in position 1), validate:
  * 
  *   a. second field is timestamp
  *   b. if 'organization', next
  *   c. if 'number', next
  */
case object EventModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.filter(isEvent(_)).flatMap(validateModel(_))
  }

  def isEvent(model: Model): Boolean = {
    model.fields.map(_.name).headOption == Some("event_id")
  }

  def validateModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "event_id" :: "timestamp" :: "organization" :: "number" :: rest => Nil

      case "event_id" :: "timestamp" :: "organization" :: rest => {
        fieldNames.contains("number") match {
          case true => Seq(error(model, "number field must come after organization in event models"))
          case false => Nil
        }
      }

      case "event_id" :: "timestamp" :: "id" :: "organization" :: "number" :: rest => Nil

      case "event_id" :: "timestamp" :: "id" :: "organization" :: rest => {
        fieldNames.contains("number") match {
          case true => Seq(error(model, "number field must come after organization in event models"))
          case false => Nil
        }
      }

      case "event_id" :: "timestamp" :: "id" :: rest => {
        validateOrgAndNumber(model, rest, "id")
      }

      case "event_id" :: "timestamp" :: rest => {
        validateOrgAndNumber(model, rest, "timestamp")
      }

      case _ => {
        val timestampErrors = fieldNames.contains("timestamp") match {
          case true => error(model, "timestamp field must come after event_id in event models")
          case false => error(model, "timestamp field is required in event models")
        }

        Seq(timestampErrors) ++ validateOrgAndNumber(model, fieldNames, "timestamp")
      }
    }
  }

  private[this] def validateOrgAndNumber(model: Model, fieldNames: Seq[String], priorFieldName: String): Seq[String] = {
    fieldNames.contains("organization") match {
      case true => Seq(error(model, s"organization field must come after $priorFieldName in event models"))
      case false => {
        fieldNames.contains("number") match {
          case true => Seq(error(model, "organization field is required if event model has a field named number"))
          case false => Nil
        }
      }
    }
  }

  
}
