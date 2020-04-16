package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SortParameterDefaultSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.SortParameterDefault

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

  it("Allows either default for imported types") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("io.flow.common.v0.models.organization", fields = Nil),
        Parameter(
          name = "sort",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = Some("fo")
        )
      )
    ) should be(
      Seq("Resource organizations GET /organizations: Parameter sort default expected to be[-created_at or name] and not[fo]")
    )

    Seq("-created_at", "name").foreach { sort =>
      linter.validate(
        buildService(
          Services.buildSimpleModel("io.flow.common.v0.models.organization", fields = Nil),
          Parameter(
            name = "sort",
            `type` = "string",
            location = ParameterLocation.Query,
            required = false,
            default = Some(sort)
          )
        )
      ) should be(Nil)
    }
  }

  it("Requires 'name' if name field") {
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
      Seq("Resource organizations GET /organizations: Parameter sort default expected to be[name] and not[foo]")
    )
  }
  
  it("Requires 'journal_timestamp' if path ends in /versions") {
    linter.validate(
      buildService(
        Services.buildSimpleModel("organization", fields = Seq("name")),
        Parameter(
          name = "sort",
          `type` = "string",
          location = ParameterLocation.Query,
          required = false,
          default = Some("name")
        ),
        path = "/organizations/versions"
      )
    ) should be(
      Seq("Resource organizations GET /organizations/versions: Parameter sort default expected to be[journal_timestamp] and not[name]")
    )
  }
  
}
