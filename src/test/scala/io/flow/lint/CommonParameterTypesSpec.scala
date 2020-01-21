package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CommonParameterTypesSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.CommonParameterTypes

  def buildService(
    paramName: String,
    paramType: String = "string",
    default: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    path: String = "/users"
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildSimpleModel("user")
      ),
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "user",
          plural = "users",
          method = Method.Get,
          path = path,
          responseCode = 200,
          responseType = "[user]",
          parameters = Seq(
            Parameter(
              name = paramName,
              `type` = paramType,
              location = ParameterLocation.Query,
              required = false,
              default = default,
              minimum = minimum,
              maximum = maximum
            )
          )
        )
      )
    )
  }

  it("id") {
    linter.validate(buildService("id", "[string]", None, None, Some(100))) should be(Nil)

    linter.validate(buildService("id", "[long]", Some("5"), Some(1), None)) should be(
      Seq(
        "Resource users GET /users Parameter id: Type expected[[string]] but found[[long]]",
        "Resource users GET /users Parameter id: Default should not be specified",
        "Resource users GET /users Parameter id: Minimum should not be specified",
        "Resource users GET /users Parameter id: Maximum was not specified - should be 100"
      )
    )
  }

  it("id on versions endpoint") {
    linter.validate(buildService("id", "[long]", None, None, Some(100), path = "/users/versions")) should be(Nil)
  }

  it("limit") {
    linter.validate(buildService("limit", "long", Some("25"), Some(1), Some(100))) should be(Nil)

    linter.validate(buildService("limit", "string", Some("5"), Some(2), Some(10))) should be(
      Seq(
        "Resource users GET /users Parameter limit: Type expected[long] but found[string]",
        "Resource users GET /users Parameter limit: Default expected[25] but found[5]",
        "Resource users GET /users Parameter limit: Minimum expected[1] but found[2]",
        "Resource users GET /users Parameter limit: Maximum expected[100] but found[10]"
      )
    )

    linter.validate(buildService("limit", "long", None, None, None)) should be(
      Seq(
        "Resource users GET /users Parameter limit: Default was not specified - should be 25",
        "Resource users GET /users Parameter limit: Minimum was not specified - should be 1",
        "Resource users GET /users Parameter limit: Maximum was not specified - should be 100"
      )
    )
  }

  it("offset") {
    linter.validate(buildService("offset", "long", Some("0"), Some(0), None)) should be(Nil)

    linter.validate(buildService("offset", "string", Some("5"), Some(2), Some(10))) should be(
      Seq(
        "Resource users GET /users Parameter offset: Type expected[long] but found[string]",
        "Resource users GET /users Parameter offset: Default expected[0] but found[5]",
        "Resource users GET /users Parameter offset: Minimum expected[0] but found[2]",
        "Resource users GET /users Parameter offset: Maximum should not be specified"
      )
    )

    linter.validate(buildService("offset", "long", None, None, None)) should be(
      Seq(
        "Resource users GET /users Parameter offset: Default was not specified - should be 0",
        "Resource users GET /users Parameter offset: Minimum was not specified - should be 0"
      )
    )
  }

}
