package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ErrorUnionModelsSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.ErrorModelsV1

  private[this] val messages = Services.buildField("messages", "[string]", minimum = Some(1))

  def buildService(
    fields: Seq[Field],
    modelName: String = "no_inventory_reservation_error",
    unionName: String = "reservation_error",
    discriminator: Option[String] = Some("code")
  ): Service = {
    Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = unionName,
          discriminator = discriminator,
          types = Seq(
            UnionType(`type` = modelName)
          )
        )
      ),
      models = Seq(
        Services.buildModel(
          name = modelName,
          fields = fields,
          attributes = Seq(
            Services.buildAttribute("linter", Json.obj("error_version" -> "1"))
          )
        )
      )
    )
  }

  it("Must have a discriminator named 'code' and associated models must have a field in position 0 named 'messages' of type '[string]'") {
    linter.validate(buildService(Seq(messages))) should be(Nil)

    val itemNumbers = Services.buildField("item_numbers", "[string]")
    linter.validate(buildService(Seq(messages, itemNumbers))) should be(Nil)

    linter.validate(buildService(Seq(itemNumbers, messages))) should be(
      Seq(
        "Model no_inventory_reservation_error: second field must be 'messages'"
      )
    )

    linter.validate(buildService(Nil)) should be(Seq(
      "Model no_inventory_reservation_error: requires a field named 'messages'"
    ))
  }

  it("messages must have a minimum >= 1") {
    linter.validate(buildService(Seq(messages.copy(minimum=None)))) should be(Seq(
      "Model no_inventory_reservation_error Field[messages]: missing minimum"
    ))
    linter.validate(buildService(Seq(messages.copy(minimum=Some(0))))) should be(Seq(
      "Model no_inventory_reservation_error Field[messages]: minimum must be >= 1"
    ))
  }

  it("error union types MUST contain only models") {
    linter.validate(
      Services.Base.copy(
        unions = Seq(
          Services.buildUnion(
            name = "reservation_error",
            discriminator = Some("code"),
            types = Seq(
              UnionType(`type` = "string")
            )
          )
        )
      )
    ) should be(
      Seq("Union reservation_error type string: Type must refer to a model to be part of an 'error' union type")
    )
  }

  it("All error union types must end in _error as well") {
    linter.validate(buildService(Seq(messages), modelName = "foo")) should be(Seq(
      "Union reservation_error type foo: Model name must end with '_error'"
    ))
  }

  it("error model that belongs to a non error union type should still get validated") {
    linter.validate(buildService(Seq(messages), unionName = "other")) should be(
      Seq(
        "Model no_inventory_reservation_error: requires a field named 'code'"
      )
    )
  }
}
