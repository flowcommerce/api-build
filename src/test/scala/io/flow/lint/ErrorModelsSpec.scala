package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ErrorModelsSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.ErrorModels

  private[this] val code = Services.buildField("code")
  private[this] val messages = Services.buildField("messages", "[string]", minimum = Some(1))

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

  it("standalone model fields must start with code, messages") {
    linter.validate(buildService(Seq(code, messages))) should be(Nil)

    linter.validate(buildService(Nil)) should be(Seq(
      "Model test_error: requires a field named 'code'",
      "Model test_error: requires a field named 'messages'"
    ))

    linter.validate(buildService(Seq(messages))) should be(Seq(
      "Model test_error: requires a field named 'code'"
    ))
    linter.validate(buildService(Seq(code))) should be(Seq(
      "Model test_error: requires a field named 'messages'"
    ))
    linter.validate(buildService(Seq(messages, code))) should be(Seq(
      "Model test_error: first field must be 'code'",
      "Model test_error: second field must be 'messages'"
    ))
  }

  it("standalone model validates type of 'code' field") {
    linter.validate(buildService(Seq(code.copy(`type` = "integer"), messages))) should be(Seq(
      "Model test_error Field[code]: type must be 'string' or a valid enum"
    ))
    linter.validate(buildService(Seq(code, messages.copy(`type` = "integer")))) should be(Seq(
      "Model test_error Field[messages]: type must be '[string]'"
    ))
  }

  it("messages must have a minimum >= 1") {
    linter.validate(buildService(Seq(code, messages.copy(minimum=None)))) should be(Seq(
      "Model test_error Field[messages]: missing minimum"
    ))
    linter.validate(buildService(Seq(code, messages.copy(minimum=Some(0))))) should be(Seq(
      "Model test_error Field[messages]: minimum must be >= 1"
    ))
  }

  it("code can be an enum") {
    val baseService = buildService(Seq(code.copy(`type` = "error_code"), messages))
    linter.validate(baseService) should be(Seq(
      "Model test_error Field[code]: type must be 'string' or a valid enum"
    ))

    val errorCode = Services.buildEnum("error_code", "error_codes")
    linter.validate(baseService.copy(enums = Seq(errorCode))) should be(Nil)
  }

  it("code can be imported enum") {
    val baseService = buildService(Seq(code.copy(`type` = "io.flow.error.v0.enums.generic_error"), messages)).copy(
      imports = Seq(Import(
        namespace = "io.flow.error.v0",
        uri = "https://app.apibuilder.io/flow/error/latest/service.json",
        organization = Organization("flow"),
        application = Application("error"),
        version = "0.0.1",
        enums = Seq("generic_error")
      ))
    )
    linter.validate(baseService) should be(Nil)
  }
  
}
