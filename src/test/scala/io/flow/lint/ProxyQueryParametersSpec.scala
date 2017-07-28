package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class ProxyQueryParametersSpec extends FunSpec with Matchers {

  private[this] val linter = linters.ProxyQueryParameters

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
    Seq("callback", "envelope", "method").foreach { word =>
      linter.validate(
        buildService(word)
      ) should be(
        Seq(s"Resource users GET /users Parameter $word: name is reserved for use only in https://github.com/flowvault/proxy")
      )
    }
  }

}
