package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class GetWithoutExpansionsSpec extends FunSpec with Matchers {

  val linter = linters.Get

  val model = Services.buildSimpleModel(
    "organization",
    fields = Seq("id", "name")
  )

  val idParameter = Parameter(
    name = "id",
    `type` = "[string]",
    location = ParameterLocation.Query,
    required = false,
    maximum = Some(100)
  )

  val limitParameter = Parameter(
    name = "limit",
    `type` = "long",
    location = ParameterLocation.Query,
    required = false,
    default = Some("25"),
    minimum = Some(1),
    maximum = Some(100)
  )

  val offsetParameter = Parameter(
    name = "offset",
    `type` = "long",
    location = ParameterLocation.Query,
    required = false,
    default = Some("0"),
    minimum = Some(0)
  )

  val sortParameter = Parameter(
    name = "sort",
    `type` = "string",
    location = ParameterLocation.Query,
    required = false,
    default = Some("created_at")
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

  def buildResourceWithSearchNonCrud(params: Seq[Parameter]) = {
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
                Services.buildResponse(`type` = "[io.flow.organization.v0.models.organization]")
              ),
              attributes = Seq(Attribute("non-crud", Json.obj()))
            )
          )
        )
      )
    )
  }

  it("GET / validation") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "id",
            `type` = "[string]",
            location = ParameterLocation.Query,
            required = true
          )
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Missing parameters: limit, offset, sort",
        "Resource organizations GET /organizations: Parameter[id] must be optional"
      )
    )
  }

  it("GET / validates limit") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "limit",
            `type` = "string",
            location = ParameterLocation.Query,
            required = true
          )
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Missing parameters: id, offset, sort",
        "Resource organizations GET /organizations: Parameter[limit] must be optional"
      )
    )
  }

  it("GET / validates offset") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "offset",
            `type` = "int",
            location = ParameterLocation.Query,
            required = true,
            minimum = Some(1),
            maximum = Some(100)
          )
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Missing parameters: id, limit, sort",
        "Resource organizations GET /organizations: Parameter[offset] must be optional"
      )
    )
  }

  it("GET / validates id not required") {
    linter.validate(
      buildResourceWithSearchNonCrud(
        Seq(
          Parameter(
            name = "limit",
            `type` = "string",
            location = ParameterLocation.Query,
            required = false
          ),
          Parameter(
            name = "offset",
            `type` = "int",
            location = ParameterLocation.Query,
            required = false,
            minimum = Some(1),
            maximum = Some(100)
          ),
          Parameter(
            name = "sort",
            `type` = "string",
            location = ParameterLocation.Query,
            required = false
          )
        )
      )
    ) should be(Nil)
  }

  it("GET / with required parameters in different order") {
    val otherParameter = Parameter(
      name = "other",
      `type` = "string",
      location = ParameterLocation.Query,
      required = false
    )

    linter.validate(
      buildResourceWithSearch(
        Seq(
          otherParameter,
          idParameter,
          limitParameter,
          offsetParameter,
          sortParameter
        )
      )
    ) should be(Seq(
      "Resource organizations GET /organizations: Parameter[id] must be the first parameter"
    ))

    linter.validate(
      buildResourceWithSearch(
        Seq(
          idParameter,
          limitParameter,
          offsetParameter,
          sortParameter,
          otherParameter
        )
      )
    ) should be(Seq(
      "Resource organizations GET /organizations: Last 3 parameters must be limit, offset, sort and not offset, sort, other"
    ))
  }

  it("GET / with valid parameters") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          idParameter,
          limitParameter,
          offsetParameter,
          sortParameter
        )
      )
    ) should be(Nil)
  }

}
