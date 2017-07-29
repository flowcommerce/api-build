package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Model, Service}
import io.flow.lint.linters.CommonParameterTypes.ignored

/**
  * For event models (models ending with 'upserted', 'deleted'), validate:
  * 
  *   a. second field is timestamp
  *   b. if 'organization', next
  *   c. if 'number', next
  */
case object EventModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.
      filter(m => !ignored(m.attributes, "event_model")).
      filter(isEvent).
      flatMap(validateModel)
  }

  private[this] val Suffixes = List(
    "upserted", "deleted"
  )
  
  private[this] def isEvent(model: Model): Boolean = {
    Suffixes.exists { s => model.name.endsWith(s"_$s") }
  }

  private[this] def validateModel(model: Model): Seq[String] = {
    validateFieldNames(model) ++ validateFieldTypes(
      model,
      Map(
        "event_id" -> "string",
        "timestamp" -> "date-time-iso8601",
        "organization" -> "string",
        "number" -> "string"
      )
    )
  }

  private[this] def validateFieldNames(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "event_id" :: "timestamp" :: "organization" :: "number" :: _ => Nil

      case "event_id" :: "timestamp" :: "organization" :: _ => {
        if (fieldNames.contains("number")) {
          Seq(error(model, "number field must come after organization in event models"))
        } else {
          Nil
        }
      }

      case "event_id" :: "timestamp" :: "id" :: "organization" :: "number" :: _ => Nil

      case "event_id" :: "timestamp" :: "id" :: "organization" :: _ => {
        if (fieldNames.contains("number")) {
          Seq(error(model, "number field must come after organization in event models"))
        } else {
          Nil
        }
      }

      case "event_id" :: "timestamp" :: "id" :: rest => {
        validateOrgAndNumber(model, rest, "id")
      }

      case "event_id" :: "timestamp" :: rest => {
        validateOrgAndNumber(model, rest, "timestamp")
      }

      case _ => {
        val eventIdErrors = if (fieldNames.headOption.contains("event_id")) {
          Nil
        } else {
          Seq(error(model, "event_id must be the first field in event models"))
        }

        val timestampErrors = if (fieldNames.contains("timestamp")) {
          error(model, "timestamp field must come after event_id in event models")
        } else {
          error(model, "timestamp field is required in event models")
        }

        eventIdErrors ++ Seq(timestampErrors) ++ validateOrgAndNumber(model, fieldNames, "timestamp")
      }
    }
  }

  private[this] def validateOrgAndNumber(model: Model, fieldNames: Seq[String], priorFieldName: String): Seq[String] = {
    if (fieldNames.contains("organization")) {
      Seq(error(model, s"organization field must come after $priorFieldName in event models"))
    } else if (fieldNames.contains("number")) {
      Seq(error(model, "organization field is required if event model has a field named number"))
    } else {
      Nil
    }
  }

  
}
