package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import org.scalatest.{FunSpec, Matchers}

class PublishedEventModelsSpec extends FunSpec with Matchers {

  private[this] val linter = linters.PublishedEventModels

  private[this] def buildService(
    fields: Map[String, String]
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = "organization_rates_published",
          fields = fields.map { case (name, typ) => Services.buildField(name = name, `type` = typ) }.toList
        )
      )
    )
  }

  it("no-op w/ invalid fields") {
    linter.validate(
      buildService(
        Map(
          "id" -> "string",
          "email" -> "string"
        )
      )
    ) should be(
      Seq(
        "Model organization_rates_published: Published event models must contain exactly four fields: event_id, timestamp, organization, data. Your model was defined as: id, email"
      )
    )
  }

  it("w/ valid field types") {
    linter.validate(
      buildService(
        Map(
          "event_id" -> "string",
          "timestamp" -> "date-time-iso8601",
          "organization" -> "string",
          "data" -> "organization_rates_data"
        )
      )
    ) should be(Nil)
  }

  it("w/ invalid field types") {
    linter.validate(
      buildService(
        Map(
          "event_id" -> "long",
          "timestamp" -> "boolean",
          "organization" -> "integer",
          "data" -> "string"
        )
      )
    ) should be(
      Seq(
        "Model organization_rates_published Field[event_id]: type must be 'string' and not long",
        "Model organization_rates_published Field[timestamp]: type must be 'date-time-iso8601' and not boolean",
        "Model organization_rates_published Field[organization]: type must be 'string' and not integer",
        "Model organization_rates_published Field[data]: type must be 'organization_rates_data' and not string"
      )
    )
  }
}
