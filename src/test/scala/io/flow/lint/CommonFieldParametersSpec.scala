package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class CommonFieldParametersSpec extends FunSpec with Matchers {

  val linter = linters.CommonFieldParameters

  def buildService(
    fieldName: String,
    default: Option[String],
    minimum: Option[Long],
    maximum: Option[Long]
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "user",
          Seq(
            Services.buildField(
              name = fieldName,
              `type` = "string",
              default = default,
              minimum = minimum,
              maximum = maximum
            )
          )
        )
      )
    )
  }

  it("id") {
    linter.validate(buildService("id", None, Some(0), Some(100))) should be(Nil)

    linter.validate(buildService("id", Some("5"), None, None)) should be(
      Seq(
        "Model user Field[id]: Default should not be specified",
        "Model user Field[id]: Minimum was not specified - should be 0",
        "Model user Field[id]: Maximum was not specified - should be 100"
      )
    )

    linter.validate(buildService("id", None, Some(1), Some(10))) should be(
      Seq(
        "Model user Field[id]: Minimum expected[0] but found[1]",
        "Model user Field[id]: Maximum expected[100] but found[10]"
      )
    )
  }

  it("limit") {
    linter.validate(buildService("limit", Some("25"), Some(1), Some(100))) should be(Nil)

    linter.validate(buildService("limit", None, None, None)) should be(
      Seq(
        "Model user Field[limit]: Default was not specified - should be 25",
        "Model user Field[limit]: Minimum was not specified - should be 1",
        "Model user Field[limit]: Maximum was not specified - should be 100"
      )
    )

    linter.validate(buildService("limit", Some("10"), Some(0), Some(10))) should be(
      Seq(
        "Model user Field[limit]: Default expected[25] but found[10]",
        "Model user Field[limit]: Minimum expected[1] but found[0]",
        "Model user Field[limit]: Maximum expected[100] but found[10]"
      )
    )
  }

  it("offset") {
    linter.validate(buildService("offset", Some("0"), Some(0), None)) should be(Nil)

    linter.validate(buildService("offset", None, None, Some(100))) should be(
      Seq(
        "Model user Field[offset]: Default was not specified - should be 0",
        "Model user Field[offset]: Minimum was not specified - should be 0",
        "Model user Field[offset]: Maximum should not be specified"
      )
    )

    linter.validate(buildService("offset", Some("10"), Some(1), Some(10))) should be(
      Seq(
        "Model user Field[offset]: Default expected[0] but found[10]",
        "Model user Field[offset]: Minimum expected[0] but found[1]",
        "Model user Field[offset]: Maximum should not be specified"
      )
    )
  }

}
