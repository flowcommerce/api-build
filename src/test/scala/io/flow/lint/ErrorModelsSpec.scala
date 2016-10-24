package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class ErrorModelsSpec extends FunSpec with Matchers {

  val linter = linters.ErrorModels

  val code = Services.buildField("code")
  val messages = Services.buildField("messages", "[string]")

  def buildService(
    fields: Seq[Field]
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = "test_error",
          fields = fields
        )
      )
    )
  }

  it("fields must start with code, messages") {
    linter.validate(buildService(Seq(code, messages))) should be(Nil)

    linter.validate(buildService(Nil)) should be(Seq(
      "Model test_error: requires a field named 'code'",
      "Model test_error: requires a field named 'messages'"
    ))

    linter.validate(buildService(Seq(messages))) should be(Seq(
      "Model test_error: requires a field named 'code'",
      "Model test_error: second field must be 'messages'"
    ))
    linter.validate(buildService(Seq(code))) should be(Seq(
      "Model test_error: requires a field named 'messages'"
    ))
    linter.validate(buildService(Seq(messages, code))) should be(Seq(
      "Model test_error: first field must be 'code'",
      "Model test_error: second field must be 'messages'"
    ))
  }

  it("validated type of 'code' field") {
    linter.validate(buildService(Seq(code.copy(`type` = "integer"), messages))) should be(Seq(
      "Model test_error Field[code]: type must be 'string'"
    ))
    linter.validate(buildService(Seq(code, messages.copy(`type` = "integer")))) should be(Seq(
      "Model test_error Field[messages]: type must be '[string]'"
    ))
  }

}
