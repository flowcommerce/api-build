package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GetWithExpansionsSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.Get

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

  def buildServiceWithUnion(expandableTypeIsArray: Boolean = false): Service = {
    val card = Services.buildModel("card", Seq(
      Services.buildField("id"),
      Services.buildField("name")
    ))

    val cardReference = Services.buildModel("card_reference", Seq(
      Services.buildField("id")
    ))

    val expandableCardType = if (expandableTypeIsArray) "[expandable_card]" else "expandable_card"
    val cardAuthorization = Services.buildModel("card_authorization", Seq(
      Services.buildField("id"),
      Services.buildField("card", `type` = expandableCardType)
    ))

    val resource = Services.buildResource(
      "authorization",
      Seq(
        Services.buildSimpleOperation(
          path = "/authorizations",
          parameters = Seq(
            Services.buildParameter(name = "id"),
            Services.buildParameter(name = "limit", `type` = "long"),
            Services.buildParameter(name = "offset", `type` = "long"),
            Services.buildParameter(name = "sort"),
            Services.buildParameter(name = "expand", `type` = "[string]", maximum = Some(1), example = Some("card"))
          ),
          responseType = "[authorization]"
        )
      )
    )

    Services.Base.copy(
      models = Seq(card, cardReference, cardAuthorization),
      unions = Seq(
        Services.buildUnion(
          name = "expandable_card",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("card"),
            Services.buildUnionType("card_reference")
          )
        ),
        Services.buildUnion(
          name = "authorization",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("card_authorization")
          )
        )
      ),
      resources = Seq(resource)
    )
  }

  it("validates expandable properties inside union fields") {
    linter.validate(
      buildServiceWithUnion()
    ) should be(Nil)
  }

  it("validates expandable properties when expandable type is an array") {
    linter.validate(
      buildServiceWithUnion(expandableTypeIsArray = true)
    ) should be(Nil)
  }
}
