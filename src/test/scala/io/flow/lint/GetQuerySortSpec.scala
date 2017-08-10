package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class GetQuerySortSpec extends FunSpec with Matchers {

  private[this] val linter = linters.GetQuerySort

  private[this] val model = Services.buildSimpleModel(
    "organization",
    fields = Seq("id", "name")
  )

  private[this] val qParameter = Parameter(
    name = "q",
    `type` = "string",
    location = ParameterLocation.Query,
    required = false
  )

  private[this] val sortParameter = Parameter(
    name = "sort",
    `type` = "string",
    location = ParameterLocation.Query,
    required = false
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
                Services.buildResponse(`type` = "[organization]")
              )
            )
          )
        )
      )
    )
  }

  it("has no errors if 'q' specified without 'sort'") {
    linter.validate(
      buildResourceWithSearch(
        Seq(qParameter)
      )
    ) should be(
      Nil
    )
  }

  it("validates sort is allowed if no 'q' parameter") {
    linter.validate(
      buildResourceWithSearch(
        Seq(sortParameter)
      )
    ) should be(
      Nil
    )
  }

  it("validates sort is NOT allowed if 'q' parameter") {
    linter.validate(
      buildResourceWithSearch(
        Seq(qParameter, sortParameter)
      )
    ) should be(
      Seq("Resource organizations GET /organizations: Parameter[sort] is not supported for operations that contain a 'q' parameter")
    )
  }

}
