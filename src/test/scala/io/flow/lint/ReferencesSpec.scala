package io.flow.lint

import io.apibuilder.spec.v0.models.Attribute
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class ReferencesSpec extends FunSpec with Matchers {

  private[this] val linter = linters.References

  it("Warns on missing reference types") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildSimpleModel("authorization_reference")
        )
      )
    ) should be(
      Seq(
        "Model authorization_reference: No such type authorization. References need to be named after existing types"
      )
    )
  }

  it("Ignores valid reference types") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildSimpleModel("authorization"),
          Services.buildSimpleModel("authorization_reference")
        )
      )
    ) should be(Nil)
  }

  it("Ignores invalid reference types if specified in attributes") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildSimpleModel("authorization_reference").copy(
            attributes = Seq(
              Attribute(
                name = "linter",
                value = Json.obj(
                  "ignore" -> Json.arr("reference_type_exists")
                )
              )
            )
          )
        )
      )
    ) should be(Nil)
  }

}
