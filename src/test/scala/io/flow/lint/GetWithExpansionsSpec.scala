package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class GetWithExpansionsSpec extends FunSpec with Matchers {

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

  val expandParameter = Parameter(
    name = "expand",
    `type` = "[string]",
    location = ParameterLocation.Query,
    required = false,
    default = None
  )

  val baseParameters = Seq(idParameter, limitParameter, offsetParameter, sortParameter)

  def buildService(model: Model, params: Seq[Parameter]) = {
    Services.Base.copy(
      models = Seq(model),
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

  it("model w/out expansions does not allow expand parameter") {
    linter.validate(
      buildService(
        model,
        baseParameters
      )
    ) should be(Nil)

    linter.validate(
      buildService(
        model,
        baseParameters ++ Seq(expandParameter)
      )
    ) should be(
      Seq("Resource organizations GET /organizations: There are no expansions available - should not have a parameter named expand")
    )
  }

  val serviceWithExpansion = buildService(
    Services.buildModel(
      "organization",
      fields = Seq(
        Services.buildField("id"),
        Services.buildField("user", "io.flow.common.v0.models.expandable_user")
      )
    ),
    baseParameters ++ Seq(expandParameter)
  )

  it("resource w/ expansions validates attributes") {
    linter.validate(serviceWithExpansion) should be(
      Seq(
        "Resource organizations GET /organizations: parameter[expand] is missing example. It must be user",
        "Resource organizations GET /organizations: parameter[expand] is missing maximum. It must be 1"
      )

    )
  }
  
}
