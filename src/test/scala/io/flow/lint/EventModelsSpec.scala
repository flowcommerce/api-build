package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class EventModelsSpec extends FunSpec with Matchers {

  private[this] val linter = linters.EventModels

  private[this] def buildService(
    fields: Seq[String]
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = "org_upserted",
          fields = fields.map { case (name) =>
            val typ = name match {
              case "timestamp" => "date-time-iso8601"
              case _ => "string"
            }
            Services.buildField(name = name, `type` = typ)
          }
        )
      )
    )
  }

  it("no-op w/out event_id") {
    linter.validate(buildService(Seq("id", "email"))) should be(
      Seq(
        "Model org_upserted: event_id must be the first field in event models",
        "Model org_upserted: timestamp field is required in event models"
      )
    )
  }

  it("fields") {
    linter.validate(buildService(Seq("event_id"))) should be(Seq(
      "Model org_upserted: timestamp field is required in event models"
    ))

    linter.validate(buildService(Seq("event_id", "foo", "timestamp"))) should be(Seq(
      "Model org_upserted: timestamp field must come after event_id in event models"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "number"))) should be(Seq(
      "Model org_upserted: organization field is required if event model has a field named number"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp", "organization"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "foo", "organization"))) should be(Seq(
      "Model org_upserted: organization field must come after timestamp in event models"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp", "id", "foo", "organization"))) should be(Seq(
      "Model org_upserted: organization field must come after id in event models"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp", "id", "organization", "number"))) should be(Nil)
    linter.validate(buildService(Seq("event_id", "timestamp", "id", "organization", "number", "foo"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "id", "organization", "foo", "number"))) should be(Seq(
      "Model org_upserted: number field must come after organization in event models"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp", "organization", "number"))) should be(Nil)
    linter.validate(buildService(Seq("event_id", "timestamp", "organization", "number", "foo"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "organization", "foo", "number"))) should be(Seq(
      "Model org_upserted: number field must come after organization in event models"
    ))
  }

}
