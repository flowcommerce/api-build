package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class CommonFieldTypesSpec extends FunSpec with Matchers {

  val linter = linters.CommonFieldTypes

  def buildService(
    fieldName: String,
    fieldType: String
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "user",
          Seq(
            Services.buildField(
              name = fieldName,
              `type` = fieldType
            )
          )
        )
      )
    )
  }

  it("id must be a string") {
    linter.validate(buildService("id", "string")) should be(Nil)
    linter.validate(buildService("id", "long")) should be(
      Seq("Model user Field[id]: Type must be 'string' and not long")
    )
  }

  it("name must be a string") {
    linter.validate(buildService("name", "string")) should be(Nil)
    linter.validate(buildService("name", "long")) should be(
      Seq("Model user Field[name]: Type must be 'string' and not long")
    )
  }

}
