package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GetByIdIsExpandableSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.GetByIdIsExpandable

  def buildService(
    responseType: String = "[expandable_organization]",
    params: Seq[Parameter] = Nil
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "organization",
          fields = Seq(
            Services.buildField("id"),
            Services.buildField("user", "io.flow.common.v0.models.expandable_user")
          )
        )
      ),
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
                Services.buildResponse(`type` = responseType)
              )
            )
          )
        )
      )
    )
  }

  it("resource that does not return an expansion") {
    linter.validate(buildService(responseType = "[organization]")) should be(Nil)
  }

  it("resource that returns an expansion") {
    linter.validate(buildService(responseType = "[expandable_organization]")) should be(
      Seq("Resource organizations GET /organizations: Missing parameter named expand")
    )

    linter.validate(buildService(responseType = "[io.flow.common.v0.models.expandable_organization]")) should be(
      Seq("Resource organizations GET /organizations: Missing parameter named expand")
    )
  }

  it("resource that returns an expansion with an expand param") {
    linter.validate(
      buildService(
        params = Seq(
          Parameter(
            name = "expand",
            `type` = "[string]",
            location = ParameterLocation.Query,
            required = false
          )
        )
      )
    ) should be(Nil)
  }

}
