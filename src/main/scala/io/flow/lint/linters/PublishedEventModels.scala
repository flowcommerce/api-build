package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Model, Service}
import io.flow.lint.Linter

/** Published event models must look like:
  *
  * "organization_rates_published": { "fields": [ { "name": "event_id", "type": "string" }, { "name": "timestamp",
  * "type": "date-time-iso8601" }, { "name": "organization", "type": "string" }, { "name": "data", "type":
  * "organization_rates_data" } ] }
  */
case object PublishedEventModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models
      .filter(m => !ignored(m.attributes, "published_event_model"))
      .filter(isPublishedEvent)
      .flatMap(validateModel)
  }

  private[this] val Suffixes = List(
    "published",
  )

  private[this] def isPublishedEvent(model: Model): Boolean = {
    Suffixes.exists { s => model.name.endsWith(s"_$s") }
  }

  def validateModel(model: Model): Seq[String] = {
    model.fields.map(_.name).toList match {
      case "event_id" :: "timestamp" :: "organization" :: "data" :: Nil => {
        validateTypes(model)
      }

      case other => {
        Seq(
          error(
            model,
            "Published event models must contain exactly four fields: event_id, timestamp, organization, data. " +
              s"Your model was defined as: ${other.mkString(", ")}",
          ),
        ) ++ validateTypes(model)
      }
    }
  }

  private[this] def validateTypes(model: Model): Seq[String] = {
    val dataTypeName = model.name.split("_").dropRight(1).mkString("_") + "_data"
    validateFieldTypes(
      model,
      Map(
        "event_id" -> "string",
        "timestamp" -> "date-time-iso8601",
        "organization" -> "string",
        "data" -> dataTypeName,
      ),
    )
  }

}
