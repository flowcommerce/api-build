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

  it("limit") {
    linter.validate(buildService("limit", Some("25"), Some(1), Some(100))) should be(Nil)

    linter.validate(buildService("limit", None, None, None)) should be(
      Seq(
        "Model user Field[limit]: Default should not be specified",
        "Model user Field[limit]: Minimum should not be specified",
        "Model user Field[limit]: Maximum should not be specified"
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

}
