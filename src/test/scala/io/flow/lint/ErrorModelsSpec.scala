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
      "error models require a field named 'code'",
      "error models require a field named 'messages'"
    ))

    linter.validate(buildService(Seq(messages))) should be(Seq(
      "error models require a field named 'code'",
      "error models require the the second field to be named 'messages'"
    ))
    linter.validate(buildService(Seq(code))) should be(Seq(
      "error models require a field named 'messages'"
    ))
    linter.validate(buildService(Seq(messages, code))) should be(Seq(
      "error models require the the first field to be named 'code'",
      "error models require the the second field to be named 'messages'"
    ))
  }

  it("validated type of 'code' field") {
    linter.validate(buildService(Seq(code.copy(`type` = "integer"), messages))) should be(Seq(
      "error models require the type of the 'code' field to be 'string'"
    ))
    linter.validate(buildService(Seq(code, messages.copy(`type` = "integer")))) should be(Seq(
      "error models require the type of the 'messages' field to be '[string]'"
    ))
  }

}
