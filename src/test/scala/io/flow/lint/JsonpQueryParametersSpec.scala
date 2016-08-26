package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class JsonpQueryParametersSpec extends FunSpec with Matchers {

  val linter = linters.JsonpQueryParameters

  def buildService(
    paramName: String
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
          path = "/users",
          responseCode = 200,
          responseType = "[user]",
          parameters = Seq(
            Parameter(
              name = paramName,
              `type` = "[string]",
              location = ParameterLocation.Query,
              required = false,
              description = None
            )
          )
        )
      )
    )
  }

  it("allows non reserved words") {
    linter.validate(
      buildService("id")
    ) should be(Nil)
  }

  it("validates reserved words") {
    linter.validate(
      buildService("method")
    ) should be(
      Seq("Resource users GET /users Parameter method: name is reserved for use only in jsonp")
    )
  }

}
