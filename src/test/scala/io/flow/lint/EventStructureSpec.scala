package io.flow.lint

import io.apibuilder.spec.v0.models.{Field, Model, Service, Union, UnionType}
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
  private[this] val userEventUnion: Union = Services.buildUnion("user_event", types = Seq(
    Services.buildUnionType("user_upserted"),
    Services.buildUnionType("user_deleted"),
  ))

  private[this] def userServiceWithModels(models: Seq[Model]): Service = {
    Services.Base.copy(
      unions = Seq(userEventUnion),
      models = Seq(userModel) ++ models
    )
  }

  private[this] def buildEventModel(name: String, fields: Seq[Field]): Model = {
    Services.buildModel(
      name,
      fields = Seq(
        Services.buildField("event_id"),
        Services.buildField("timestamp", `type` = "date-time-iso8601"),
      ) ++ fields
    )
  }

  it("upserted events have matching deleted events") {
    def setup(deleteModel: Option[Model], deleteUnionType: Option[UnionType]) = {
      linter.validate(
        Services.Base.copy(
          models = Seq(userModel) ++ Seq(
            buildEventModel("user_upserted", Seq(
              Services.buildField("user", `type` = "user")
            ))
          ) ++ deleteModel.toSeq,
          unions = Seq(
            Services.buildUnion("user_event", types = Seq(
              Services.buildUnionType("user_upserted")
            ) ++ deleteUnionType.toSeq)
          )
        )
      )
    }

    setup(deleteModel = None, deleteUnionType = None) shouldBe Seq(
      "Missing delete event for 'user_upserted'"
    )

    setup(deleteModel = Some(
      buildEventModel("user_deleted", Seq(
        Services.buildField("id")
      ))
    ), deleteUnionType = Some(
      Services.buildUnionType("user_deleted")
    )) shouldBe Nil
  }

  it("upserted events have matching deleted events spanning version numbers") {
    //"user_upserted_v4" and "user_deleted_v1"
  }

  it("deleted_events must have an 'id' string field") {
    def setup(field: Field) = {
      linter.validate(
        userServiceWithModels(Seq(
          buildEventModel("user_upserted", Seq(
            Services.buildField("user", `type` = "user")
          )),
          buildEventModel("user_deleted", Seq(field))
        ))
      )
    }

    setup(Services.buildField("id", `type` = "string")) shouldBe Nil
    setup(Services.buildField("user", `type` = "user")) shouldBe Seq(
      "Deleted event 'user_deleted' is missing a field named 'id'",
      "Model user_deleted Field[user]: Invalid name 'user'. Must be one of: id, organization, channel, partner"
    )
    setup(Services.buildField("id", `type` = "object")) shouldBe Seq(
      "Model 'user_deleted' Field 'id' must have type 'string' and not 'object'"
    )
  }

  it("events have no additional fields") {
    def setup(fields: Seq[Field]) = {
      linter.validate(
        userServiceWithModels(Seq(
          buildEventModel("user_upserted", fields),
          buildEventModel("user_deleted", Seq(Services.buildField("id")))
        ))
      )
    }

    val userField = Services.buildField("user", `type` = "user")

    setup(Seq(userField)) shouldBe Nil

    setup(Seq(
      Services.buildField("organization", `type` = "string"),
      userField
    )) shouldBe Nil

    setup(Seq(
      Services.buildField("foo", `type` = "string"),
      userField
    )) shouldBe Seq("Model user_upserted Field[foo]: Invalid name 'foo'. Must be one of: user, organization, channel, partner")

    setup(Seq(
      Services.buildField("organization", `type` = "string"),
      userField,
      Services.buildField("foo", `type` = "string")
    )) shouldBe Seq("Model user_upserted: Cannot have more than 4 fields")
  }

  it("associated model must have an id field") {
  }
}
