package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Model, Service}

/**
  * For event models (those with an event_id field in position 1), validate:
  * 
  *   a. second field is timestamp
  *   b. if 'organization', next
  *   c. if 'number', next
  */
case object EventModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.filter(isEvent).flatMap(validateModel)
  }

  def isEvent(model: Model): Boolean = {
    model.fields.map(_.name).headOption.contains("event_id")
  }

  def validateModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    fieldNames match {
      case "event_id" :: "timestamp" :: "organization" :: "number" :: _ => Nil

      case "event_id" :: "timestamp" :: "organization" :: _ => {
        if (fieldNames.contains("number")) {
          Seq("number field must come after organization in event models")
        } else {
          Nil
        }
      }

      case "event_id" :: "timestamp" :: rest => {
        validateOrgAndNumber(rest)
      }

      case _ => {
        val timestampErrors = if (fieldNames.contains("timestamp")) {
          Seq("timestamp field must come after event_id in event models")
        } else {
          Seq("timestamp field is required in event models")
        }

        timestampErrors ++ validateOrgAndNumber(fieldNames)
      }
    }
  }

  private[this] def validateOrgAndNumber(fieldNames: Seq[String]): Seq[String] = {
    val orgErrors = if (fieldNames.contains("organization")) {
      Seq("organization field must come after timestamp in event models")
    } else {
      Nil
    }

    val numberErrors = if (fieldNames.contains("number")) {
      Seq("organization field is required if event model has a field named number")
    } else {
      Nil
    }

    orgErrors ++ numberErrors
  }

  
}
