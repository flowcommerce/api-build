package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BeaconEventsMustHaveAttributesSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.BeaconEventsMustHaveAttributes

  def buildModel(fieldNames: Seq[String]): Model = {
    Services.buildModel(
      "user",
      fieldNames.map( name =>
        Services.buildField(
          name = name
        )
      )
    )
  }

  def buildUnion(name: String, typeName: String): Union = {
    Services.buildUnion(
      name,
      types = Seq(
        Services.buildUnionType(typeName)
      )
    )
  }

  it("model not part of union") {
    linter.validate(
      Services.Base.copy(
        models = Seq(buildModel(Seq("id")))
      )
    ) should be(Nil)
  }

  it("model part of union") {
    val model = buildModel(Seq("id"))

    linter.validate(
      Services.Base.copy(
        models = Seq(model),
        unions = Seq(buildUnion("test", model.name))
      )
    ) should be(Nil)

    val union = buildUnion("event", model.name)
    linter.validate(
      Services.Base.copy(
        models = Seq(model),
        unions = Seq(union)
      )
    ) should be(
      Seq(
        "Model user: Must have a field named 'attributes' of type 'beacon_attributes'"
      )
    )

    linter.validate(
      Services.Base.copy(
        models = Seq(
          model.copy(
            fields = Seq(
              Services.buildField("attributes")
            )
          )
        ),
        unions = Seq(union)
      )
    ) should be(
      Seq(
        "Model user Field[attributes]: Must not be required and have type 'beacon_attributes' and not 'string'"
      )
    )

    linter.validate(
      Services.Base.copy(
        models = Seq(
          model.copy(
            fields = Seq(
              Services.buildField("attributes", required = false, `type` = "beacon_attributes")
            )
          )
        ),
        unions = Seq(union)
      )
    ) should be(Nil)
  }
}
