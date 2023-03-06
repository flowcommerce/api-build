package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EventModelsSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.EventModels
  private[this] val userModel = Services.buildModel(
    "user",
    fields = Seq(
      Services.buildField("id")
    )
  )

  it("upserted events have matching deleted events") {
    val service = Services.Base.copy(
      models = Seq(userModel),
      unions = Seq(
        Services.buildUnion("user_event", types = Seq(
          Services.buildUnionType("user_upserted"),
          Services.buildUnionType("user_deleted")
        ))
      )
    )
    linter.validate(service) shouldBe Nil
  }

  it("deleted_events must have an 'id' string field") {
  }

  it("events have no additional fields") {
  }

  it("associated model must have an id field") {
  }
}
