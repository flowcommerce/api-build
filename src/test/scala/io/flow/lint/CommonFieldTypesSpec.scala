package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CommonFieldTypesSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.CommonFieldTypes

  def buildService(
    fieldName: String,
    fieldType: String,
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "user",
          Seq(
            Services.buildField(
              name = fieldName,
              `type` = fieldType,
            ),
          ),
        ),
      ),
    )
  }

  it("id must be a string") {
    linter.validate(buildService("id", "string")) should be(Nil)
    linter.validate(buildService("id", "long")) should be(
      Seq("Model user Field[id]: Type must be 'string' and not long"),
    )
  }

  it("number must be a string") {
    linter.validate(buildService("number", "string")) should be(Nil)
    linter.validate(buildService("number", "long")) should be(
      Seq("Model user Field[number]: Type must be 'string' and not long"),
    )
  }

  it("guid must be a uuid") {
    linter.validate(buildService("guid", "uuid")) should be(Nil)
    linter.validate(buildService("guid", "string")) should be(
      Seq("Model user Field[guid]: Type must be 'uuid' and not string"),
    )
  }

  it("email must be a string") {
    linter.validate(buildService("email", "string")) should be(Nil)
    linter.validate(buildService("email", "long")) should be(
      Seq("Model user Field[email]: Type must be 'string' and not long"),
    )
  }

}
