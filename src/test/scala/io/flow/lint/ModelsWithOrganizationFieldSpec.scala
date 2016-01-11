package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class ModelsWithOrganizationFieldSpec extends FunSpec with Matchers {

  val linter = linters.ModelsWithOrganizationField

  def buildService(fields: Seq[String]): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildSimpleModel("user", fields)
      )
    )
  }

  it("no-op w/out organization field") {
    linter.validate(buildService(Seq("id", "email"))) should be(Nil)
  }

  it("w/ organization but no id fields") {
    linter.validate(buildService(Seq("other", "organization"))) should be(Seq(
      "Model user: Field[organization] must be in position[0] and not[1]"
    ))
  }

  it("w/ id and organization fields") {
    linter.validate(buildService(Seq("id", "other", "organization"))) should be(Seq(
      "Model user: Field[organization] must be in position[1] and not[2]"
    ))
  }

  it("w/ id, timestamp, type and organization fields") {
    linter.validate(buildService(Seq("id", "timestamp", "type", "other", "organization"))) should be(Seq(
      "Model user: Field[organization] must be in position[3] and not[4]"
    ))
  }

}
