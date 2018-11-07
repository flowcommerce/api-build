package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class UpsertedDeletedEventModelsSpec extends FunSpec with Matchers {

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
    linter.validate(buildService("user_upserted", "user", "user")) should be (Nil)

    linter.validate(buildService("user_upserted", "foo", "user")) should be (
      Seq("Model user_upserted: Event must contain a field whose name and type contain user")
    )
  }

  it("with partial names") {
    linter.validate(buildService("card_authorization_upserted", "card_authorization", "card_authorization")) should be(Nil)
    linter.validate(buildService("card_authorization_upserted", "card", "card_authorization")) should be(Nil)
    linter.validate(buildService("card_authorization_upserted", "authorization", "card_authorization")) should be(Nil)
    linter.validate(buildService("card_authorization_upserted", "foo", "card_authorization")) should be(
      Seq("Model card_authorization_upserted: Event must contain a field whose name and type contain card or authorization")
    )
    linter.validate(buildService("card_authorization_upserted", "card_authorization", "foo")) should be(
      Seq("Model card_authorization_upserted: Event must contain a field whose name and type contain card or authorization")
    )
  }

}
