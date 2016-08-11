package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class EventModelsSpec extends FunSpec with Matchers {

  val linter = linters.EventModels

  def buildService(fields: Seq[String]): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildSimpleModel("org_upserted", fields)
      )
    )
  }

  it("no-op w/out event_id field") {
    linter.validate(buildService(Seq("id", "email"))) should be(Nil)
  }

  it("fields") {
    linter.validate(buildService(Seq("event_id"))) should be(Seq(
      "timestamp field is required in event models"
    ))

    linter.validate(buildService(Seq("event_id", "foo", "timestamp"))) should be(Seq(
      "timestamp field must come after event_id in event models"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "number"))) should be(Seq(
      "organization field is required if event model has a field named number"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp", "organization"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "foo", "organization"))) should be(Seq(
      "organization field must come after timestamp in event models"
    ))

    linter.validate(buildService(Seq("event_id", "timestamp", "organization", "number"))) should be(Nil)
    linter.validate(buildService(Seq("event_id", "timestamp", "organization", "number", "foo"))) should be(Nil)

    linter.validate(buildService(Seq("event_id", "timestamp", "organization", "foo", "number"))) should be(Seq(
      "number field must come after organization in event models"
    ))
  }

}
