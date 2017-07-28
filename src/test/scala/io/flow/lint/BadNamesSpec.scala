package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class BadNamesSpec extends FunSpec with Matchers {

  private[this] val linter = linters.BadNames

  def buildModel(
    fieldName: String
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "user",
          Seq(
            Services.buildField(
              name = fieldName
            )
          )
        )
      )
    )
  }

  def buildResource(
    paramName: String
  ): Service = {
    buildModel("id").copy(
      resources = Seq(
        Services.buildResource(
          `type` = "user",
          plural = "users",
          operations = Seq(
            Services.buildSimpleOperation(
              path = "/users",
              responseType = "[user]",
              parameters = Seq(
                Services.buildParameter(paramName)
              )
            )
          )
        )
      )
    )
  }
  
  it("model fields") {
    linter.validate(buildModel("country_of_origin")) should be(
      Seq("Model user Field[country_of_origin]: Name must be 'origin'")
    )

    linter.validate(buildModel("ip_address")) should be(
      Seq("Model user Field[ip_address]: Name must be 'ip'")
    )

    linter.validate(buildModel("postal_code")) should be(
      Seq("Model user Field[postal_code]: Name must be 'postal'")
    )
  }

  
  it("resource parameter names") {
    linter.validate(buildResource("country_of_origin")) should be(
      Seq("Resource users GET /users Parameter country_of_origin: Name must be 'origin'")
    )

    linter.validate(buildResource("ip_address")) should be(
      Seq("Resource users GET /users Parameter ip_address: Name must be 'ip'")
    )

    linter.validate(buildResource("postal_code")) should be(
      Seq("Resource users GET /users Parameter postal_code: Name must be 'postal'")
    )
  }
  
}
