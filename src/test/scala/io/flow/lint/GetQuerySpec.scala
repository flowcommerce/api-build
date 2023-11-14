package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GetQuerySpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.Get

  val model = Services.buildSimpleModel(
    "organization",
    fields = Seq("id", "name"),
  )

  val idParameter = Parameter(
    name = "q",
    `type` = "string",
    location = ParameterLocation.Query,
    required = false,
  )

  def buildResourceWithSearch(params: Seq[Parameter]) = {
    Services.Base.copy(
      resources = Seq(
        Resource(
          `type` = "organization",
          plural = "organizations",
          operations = Seq(
            Operation(
              method = Method.Get,
              path = "/organizations",
              parameters = params,
              responses = Seq(
                Services.buildResponse(`type` = "[organization]"),
              ),
            ),
          ),
        ),
      ),
    )
  }

  it("GET / validates 'q' must be optional") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "q",
            `type` = "string",
            location = ParameterLocation.Query,
            required = true,
          ),
        ),
      ),
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Parameter[q] must be optional",
      ),
    )
  }

  it("GET / with valid resources") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "q",
            `type` = "string",
            location = ParameterLocation.Query,
            required = false,
          ),
        ),
      ),
    ) should be(Nil)
  }

}
