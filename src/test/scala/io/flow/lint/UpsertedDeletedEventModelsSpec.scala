package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
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
    linter.validate(buildService("user_upserted", Seq("id", "event_id", "user"))) should be (Nil)

    linter.validate(buildService("user_upserted", Seq("id", "event_id", "foo"))) should be (
      Seq("Model user_upserted: Event must contain a field named 'user'")
    )
  }

}
