package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class UpsertedDeletedEventModelsSpec extends FunSpec with Matchers {

  private[this] val linter = linters.UpsertedDeletedEventModels

  def buildService(modelName: String, fieldNames: Seq[String]): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = modelName,
          fieldNames.map { n => Services.buildField(name = n) }
        )
      )
    )
  }

  it("with valid names") {
    linter.validate(buildService("user_upserted", Seq("user"))) should be (Nil)

    linter.validate(buildService("user_upserted", Seq("foo"))) should be (
      Seq("Model user_upserted: Event must contain a field named 'user'")
    )
  }

}
