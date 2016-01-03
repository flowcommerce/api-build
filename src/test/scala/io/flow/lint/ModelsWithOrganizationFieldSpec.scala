package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class ModelsWithOrganizationFieldSpec extends FunSpec with Matchers {

  val linter = Lint(Seq(linters.ModelsWithOrganizationField))

  it("no-op w/out organization field") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildModel(
            name = "user",
            fields = Seq(
              Services.buildField("id"),
              Services.buildField("email")
            )
          )
        )
      )
    ) should be(Nil)
  }

  it("w/ organization but no id fields") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildModel(
            name = "user",
            fields = Seq(
              Services.buildField("other"),
              Services.buildField("organization")
            )
          )
        )
      )
    ) should be(Seq(
      "Model user: Field[operation] must be in position[0] and not[1]"
    ))
  }

  it("w/ id and organization fields") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildModel(
            name = "user",
            fields = Seq(
              Services.buildField("id"),
              Services.buildField("other"),
              Services.buildField("organization")
            )
          )
        )
      )
    ) should be(Seq(
      "Model user: Field[operation] must be in position[1] and not[2]"
    ))
  }

  it("w/ id, timestamp, type and organization fields") {
    linter.validate(
      Services.Base.copy(
        models = Seq(
          Services.buildModel(
            name = "user",
            fields = Seq(
              Services.buildField("id"),
              Services.buildField("timestamp"),
              Services.buildField("type"),
              Services.buildField("other"),
              Services.buildField("organization")
            )
          )
        )
      )
    ) should be(Seq(
      "Model user: Field[operation] must be in position[3] and not[4]"
    ))
  }

}
