package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class CommonParametersHaveNoDescriptionsSpec extends FunSpec with Matchers {

  val linter = linters.CommonParametersHaveNoDescriptions

  def buildService(
    paramName: String,
    paramDescription: Option[String] = None
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
          responseType = "[organization]",
          parameters = Seq(
            Parameter(
              name = paramName,
              `type` = "[string]",
              location = ParameterLocation.Query,
              required = false,
              description = paramDescription
            )
          )
        )
      )
    )
  }

  it("Non common parameters can have descriptions") {
    linter.validate(
      buildService("foo", None)
    ) should be(Nil)

    linter.validate(
      buildService("foo", Some("bar"))
    ) should be(Nil)
  }

  it("Common parameters cannot have descriptions") {
    linters.CommonParametersHaveNoDescriptions.NamesWithNoDescriptions.foreach { name =>
      linter.validate(
        buildService(name, Some("bar"))
      ) should be(
        Seq(s"Resource users GET /users Parameter $name: Must not have a description")
      )
    }
  }

}
