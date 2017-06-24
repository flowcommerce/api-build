package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class SortAttributeSpec extends FunSpec with Matchers {

  val linter = linters.SortAttribute

  val sortParameter = Parameter(
    name = "sort",
    `type` = "string",
    location = ParameterLocation.Query,
    required = false,
    default = None
  )

  val sortAttribute = Attribute(
    name = "sort",
    Json.obj()
  )

  val otherAttribute = Attribute(
    name = "other",
    Json.obj()
  )

  def buildService(parameter: Parameter, attribute: Attribute): Service = {
    Services.Base.copy(
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "organization",
          plural = "organizations",
          method = Method.Get,
          path = "/organization",
          responseCode = 200,
          responseType = "[organization]",
          parameters = Seq(parameter),
          attributes = Seq(attribute)
        )
      )
    )
  }

  it("should be empty when sort attribute exists") {
    linter.validate(
      buildService(sortParameter, sortAttribute)
    ) should be (Nil)
  }

  it("should have errors when sort attribute is missing") {
    linter.validate(
      buildService(sortParameter, otherAttribute)
    ) should be(Seq("Resource organizations GET /organization: Missing attribute named sort"))
  }
}
