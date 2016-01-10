package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class GetWithoutExpansionsSpec extends FunSpec with Matchers {

  val linter = Lint(Seq(linters.Get))

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
      Services.withHealthcheck(
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
        "Resource organizations GET /organizations: Parameter[id] must be optional",
        "Resource organizations GET /organizations: parameter[id] maximum is missing. Expected 100"
      )
    )
  }

  it("GET / validates id cannot have default") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "id",
            `type` = "[string]",
            location = ParameterLocation.Query,
            required = true,
            default = Some("1")
          )
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Missing parameters: limit, offset, sort",
        "Resource organizations GET /organizations: parameter[id] default must not be specified. Current value is 1",
        "Resource organizations GET /organizations: parameter[id] maximum is missing. Expected 100"
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
        "Resource organizations GET /organizations: Parameter[limit] must be optional",
        "Resource organizations GET /organizations: parameter[limit] must be of type long and not string",
        "Resource organizations GET /organizations: parameter[limit] default is missing. Expected 25",
        "Resource organizations GET /organizations: parameter[limit] minimum is missing. Expected 1",
        "Resource organizations GET /organizations: parameter[limit] maximum is missing. Expected 100"
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
        "Resource organizations GET /organizations: Parameter[offset] must be optional",
        "Resource organizations GET /organizations: parameter[offset] must be of type long and not int",
        "Resource organizations GET /organizations: parameter[offset] default is missing. Expected 0",
        "Resource organizations GET /organizations: parameter[offset] minimum must be 0 and not 1",
        "Resource organizations GET /organizations: parameter[offset] maximum must not be specified. Current value is 100"
      )
    )
  }

  it("GET / validates sort") {
    linter.validate(
      buildResourceWithSearch(
        Seq(
          Parameter(
            name = "sort",
            `type` = "int",
            location = ParameterLocation.Query,
            required = false
          )
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Missing parameters: id, limit, offset",
        "Resource organizations GET /organizations: parameter[sort] must be of type string and not int",
        "Resource organizations GET /organizations: parameter[sort] must have a default"
      )
    )
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
