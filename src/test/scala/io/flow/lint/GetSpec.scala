package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class GetSpec extends FunSpec with Matchers {

  val organization = Services.withHealthcheck(
    Services.Base.copy(
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "organization",
          plural = "organizations",
          method = Method.Get,
          path = "/organizations",
          responseCode = 200,
          responseType = "[organization]"
        )
      )
    )
  )

  val idParameter = Parameter(
    name = "id",
    `type` = "[string]",
    location = ParameterLocation.Query,
    required = false,
    maximum = Some(25)
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

  it("requires at least one GET operation for each resource") {
    Lint(
      Services.withHealthcheck(
        Services.Base.copy(
          resources = Seq(
            Resource(
              `type` = "organization",
              plural = "organizations",
              operations = Nil
            )
          )
        )
      )
    ).validate should be(
      Seq("organizations: Must have at least one operation")
    )
  }

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
    Lint(
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
    ).validate should be(
      Seq(
        "organizations GET /organizations: Missing parameters: limit, offset, sort",
        "organizations GET /organizations: Parameter[id] must be optional",
        "organizations GET /organizations: parameter[id] maximum is missing. Expected 25"
      )
    )
  }

  it("GET / validates id cannot have default") {
    Lint(
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
    ).validate should be(
      Seq(
        "organizations GET /organizations: Missing parameters: limit, offset, sort",
        "organizations GET /organizations: parameter[id] default must not be specified. Current value is 1",
        "organizations GET /organizations: parameter[id] maximum is missing. Expected 25"
      )
    )
  }

  it("GET / validates limit") {
    Lint(
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
    ).validate should be(
      Seq(
        "organizations GET /organizations: Missing parameters: id, offset, sort",
        "organizations GET /organizations: Parameter[limit] must be optional",
        "organizations GET /organizations: parameter[limit] must be of type long and not string",
        "organizations GET /organizations: parameter[limit] default is missing. Expected 25",
        "organizations GET /organizations: parameter[limit] minimum is missing. Expected 1",
        "organizations GET /organizations: parameter[limit] maximum is missing. Expected 100"
      )
    )
  }

  it("GET / validates offset") {
    Lint(
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
    ).validate should be(
      Seq(
        "organizations GET /organizations: Missing parameters: id, limit, sort",
        "organizations GET /organizations: Parameter[offset] must be optional",
        "organizations GET /organizations: parameter[offset] must be of type long and not int",
        "organizations GET /organizations: parameter[offset] default is missing. Expected 0",
        "organizations GET /organizations: parameter[offset] minimum must be 0 and not 1",
        "organizations GET /organizations: parameter[offset] maximum must not be specified. Current value is 100"
      )
    )
  }

  it("GET / validates sort") {
    Lint(
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
    ).validate should be(
      Seq(
        "organizations GET /organizations: Missing parameters: id, limit, offset",
        "organizations GET /organizations: parameter[sort] must be of type string and not int",
        "organizations GET /organizations: parameter[sort] must have a default"
      )
    )
  }
  
  it("GET / with valid parameters") {
    Lint(
      buildResourceWithSearch(
        Seq(
          idParameter,
          limitParameter,
          offsetParameter,
          sortParameter
        )
      )
    ).validate should be(Nil)
  }

}
