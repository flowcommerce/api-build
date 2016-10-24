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

  def buildServiceWithUnion(
    fields: Seq[Field],
    discriminator: Option[String] = Some("code")
  ): Service = {
    Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          "user_error",
          discriminator = discriminator,
          types = Seq(
            Services.buildUnionType("user_auth_error")
          )
        )
      ),
      models = Seq(
        Services.buildModel(
          name = "user_auth_error",
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

  it("standalone model validated type of 'code' field") {
    linter.validate(buildService(Seq(code.copy(`type` = "integer"), messages))) should be(Seq(
      "Model test_error Field[code]: type must be 'string'"
    ))
    linter.validate(buildService(Seq(code, messages.copy(`type` = "integer")))) should be(Seq(
      "Model test_error Field[messages]: type must be '[string]'"
    ))
  }

  it("unions") {
    linter.validate(buildServiceWithUnion(Seq(messages))) should be(Nil)

    linter.validate(buildServiceWithUnion(Seq(messages, code))) should be(Seq(
      "Model user_auth_error: field named 'code' must come from union type discriminator"
    ))

    linter.validate(buildServiceWithUnion(Seq(messages.copy(`type` = "integer")))) should be(Seq(
      "Model user_auth_error Field[messages]: type must be '[string]'"
    ))

    linter.validate(buildServiceWithUnion(Seq(messages.copy(name = "foo")))) should be(Seq(
      "Model user_auth_error: requires a field named 'messages'"
    ))

    linter.validate(buildServiceWithUnion(Seq(code.copy(name = "foo"), messages))) should be(Seq(
      "Model user_auth_error: first field must be 'messages'"
    ))
  }

}
