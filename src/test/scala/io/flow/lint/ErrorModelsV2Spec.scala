package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ErrorModelsV2Spec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.ErrorModelsV2

  private[this] val codeEnum = Services.buildEnum("checkout_error_code", values = Seq(
    Services.buildEnumValue("unknown")
  ))
  private[this] val code = Services.buildField("code", "checkout_error_code")
  private[this] val message = Services.buildField("message")
  private[this] val error = Services.buildModel(
    "checkout_error",
    fields = Seq(code, message),
  )
  private[this] val errors = Services.buildModel(
    "checkout_errors",
    fields = Seq(
      Services.buildField("errors", s"[${error.name}]")
    )
  )

  def buildService(models: Seq[Model]): Service = {
    Services.Base.copy(
      enums = Seq(codeEnum),
      models = models.map { m =>
        m.copy(
          attributes = Seq(
            Services.buildAttribute("linter", Json.obj(
              "error_version" -> "2"
            ))
          )
        )
      }
    )
  }

  it("errors must contain field named 'errors'") {
    linter.validate(buildService(
      Seq(Services.buildModel("test_errors"))
    )) should be(Seq(
      "Model test_errors: must contain a field named 'errors'",
    ))

    linter.validate(buildService(Seq(errors))) should be(Nil)
  }

}
