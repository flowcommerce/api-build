package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class SortParameterDefaultSpec extends FunSpec with Matchers {

  val linter = Lint(Seq(linters.SortParameterDefault))

  def buildService(model: Model, param: Parameter, path: String = "/organizations"): Service = {
    Services.Base.copy(
      models = Seq(model),
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "organization",
          plural = "organizations",
          method = Method.Get,
          path = path,
          responseCode = 200,
          responseType = "[organization]",
          parameters = Seq(param)
        )
      )
    )
  }

  val sortParameter = Parameter(
    name = "sort",
    `type` = "string",
    location = ParameterLocation.Query,
    required = false,
    default = None
  )

  it("No-op if no sort parameter") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("organization", fields = Nil),
        Parameter(
          name = "id",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = None
        )
      )
    ) should be(Nil)
  }

  it("Requires a default is present") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("organization", fields = Nil),
        Parameter(
          name = "sort",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = None
        )
      )
    ) should be(
      Seq("Resource organizations GET /organizations: Parameter sort requires a default")
    )
  }

  it("Requires '-created_at' if no name field") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("organization", fields = Nil),
        Parameter(
          name = "sort",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = Some("foo")
        )
      )
    ) should be(
      Seq("Resource organizations GET /organizations: Parameter sort default expected to be[-created_at] and not[foo]")
    )
  }

  it("Requires 'lower(name),-created_at' if name field") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("organization", fields = Seq("name")),
        Parameter(
          name = "sort",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = Some("foo")
        )
      )
    ) should be(
      Seq("Resource organizations GET /organizations: Parameter sort default expected to be[lower(name),-created_at] and not[foo]")
    )
  }
  
  it("Requires 'created_at' if path ends in /versions") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("organization", fields = Seq("name")),
        Parameter(
          name = "sort",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = Some("lower(name),-created_at")
        ),
        path = "/organizations/versions"
      )
    ) should be(
      Seq("Resource organizations GET /organizations/versions: Parameter sort default expected to be[created_at] and not[lower(name),-created_at]")
    )
  }
  
}
