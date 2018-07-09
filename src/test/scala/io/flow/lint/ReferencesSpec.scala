package io.flow.lint

import org.scalatest.{FunSpec, Matchers}

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

}
