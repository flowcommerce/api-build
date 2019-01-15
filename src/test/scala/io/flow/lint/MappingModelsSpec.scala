package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class MappingModelsSpec extends FunSpec with Matchers {

  private[this] val linter = linters.MappingModels

  private[this] val id = Services.buildField("id", "string")
  private[this] val experienceReference = Services.buildField("experience", "experience_reference")
  private[this] val priceBookReference = Services.buildField("price_book", "price_book_reference")
  private[this] val position = Services.buildField("position", "long")


  def buildService(
    name: String = "experience_price_book_mapping",
    fields: Seq[Field]
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = name,
          fields = fields
        )
      )
    )
  }

  it("validates model name") {
    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, experienceReference, priceBookReference)
      )
    ) should be(Nil)

    linter.validate(
      buildService(
        name = "price_book_experience_mapping",
        fields = Seq(id, experienceReference, priceBookReference)
      )
    ) should be(
      Seq("Model 'price_book_experience_mapping' must be named 'experience_price_book_mapping'")
    )
  }

  it("requires at least 3 fields") {
    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id)
      )
    ) should be(
      Seq("Model experience_price_book_mapping: Mapping models must have at least 3 fields")
    )

    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, experienceReference)
      )
    ) should be(
      Seq("Model experience_price_book_mapping: Mapping models must have at least 3 fields")
    )
  }

  it("allows additional fields") {
    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, experienceReference, priceBookReference, position)
      )
    ) should be(Nil)
  }

  it("validates types") {
    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, position, experienceReference, priceBookReference)
      )
    ) should be(
      Seq("Field 'position' type must be 'position_reference'")
    )
  }

  it("validates names") {
    val other = Services.buildField("foo", "order_reference")
    val fullyQualified = Services.buildField("price_book", "io.flow.price.v0.models.price_book_reference")

    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, other, experienceReference, priceBookReference)
      )
    ) should be(
      Seq("Field 2 'foo' must be named 'order'")
    )

    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, experienceReference, fullyQualified)
      )
    ) should be(
      Nil
    )
  }

  it("validates field types are proper references") {
    linter.validate(
      buildService(
        name = "experience_price_book_mapping",
        fields = Seq(id, experienceReference, priceBookReference)
      )
    ) should be(Nil)

    linter.validate(
      buildService(
        name = "price_book_experience_mapping",
        fields = Seq(
          id,
          Services.buildField("experience", "string"),
          priceBookReference
        )
      )
    ) should be(
      Seq("Field 'experience' type must be 'experience_reference'")
    )
  }

}