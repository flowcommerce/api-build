package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class UpsertedDeletedEventModelsSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.UpsertedDeletedEventModels

  def buildService(modelName: String, fieldName: String, fieldType: String): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = modelName,
          Seq(Services.buildField(name = fieldName, `type` = fieldType))
        )
      )
    )
  }

  it("with valid names") {
    linter.validate(buildService("example_upserted", "example", "example")) should be(Nil)

    linter.validate(buildService("example_upserted", "foo", "example")) should be(
      Seq("Model example_upserted: Event must contain a field whose name and type contain example")
    )
  }

  it("with partial names") {
    linter.validate(buildService("card_authorization_upserted", "card_authorization", "card_authorization")) should be(
      Nil
    )
    linter.validate(buildService("card_authorization_upserted", "card", "card_authorization")) should be(Nil)
    linter.validate(buildService("card_authorization_upserted", "authorization", "card_authorization")) should be(Nil)
    linter.validate(buildService("card_authorization_upserted", "foo", "card_authorization")) should be(
      Seq(
        "Model card_authorization_upserted: Event must contain a field whose name and type contain card or authorization"
      )
    )
    linter.validate(buildService("card_authorization_upserted", "card_authorization", "foo")) should be(
      Seq(
        "Model card_authorization_upserted: Event must contain a field whose name and type contain card or authorization"
      )
    )
    linter.validate(buildService("card_authorization_deleted", "card_authorization", "foo")) should be(
      Seq(
        "Model card_authorization_deleted: Event must contain a field whose name and type contain card or authorization"
      )
    )
  }

  it("deleted events can just use 'id' w/ type string") {
    linter.validate(buildService("card_authorization_deleted", "id", "string")) should be(Nil)
    linter.validate(buildService("card_authorization_deleted", "id", "object")) should be(
      Seq("Model card_authorization_deleted: Type of field 'id' must be 'string' and not 'object'")
    )
  }

  it("ignores legacy models") {
    linter.validate(buildService("item_origin_deleted", "foo", "item_origin")) should be(Nil)
  }

}
