package io.flow.lint

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EventStructureSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.EventStructure
  private[this] val userModel = Services.buildModel(
    "user",
    fields = Seq(
      Services.buildField("id")
    )
  )

  it("upserted events have matching deleted events") {
    val service = Services.Base.copy(
      models = Seq(userModel) ++ Seq(
        Services.buildModel(
          "user_upserted",
          fields = Seq(
            Services.buildField("event_id"),
            Services.buildField("timestamp", `type` = "date-time-iso8601"),
            Services.buildField("user", `type` = "user")
          )
        ),
        Services.buildModel(
          "user_deleted",
          fields = Seq(
            Services.buildField("event_id"),
            Services.buildField("timestamp", `type` = "date-time-iso8601"),
            Services.buildField("id", `type` = "string")
          )
        )
      ),
      unions = Seq(
        Services.buildUnion("user_event", types = Seq(
          Services.buildUnionType("user_upserted"),
          Services.buildUnionType("user_deleted")
        ))
      )
    )
    linter.validate(service) shouldBe Nil
  }

  it("upserted events have matching deleted events spanning version numbers") {
    //"user_upserted_v4" and "user_deleted_v1"
  }

    it("deleted_events must have an 'id' string field") {
  }

  it("events have no additional fields") {
  }

  it("associated model must have an id field") {
  }
}
