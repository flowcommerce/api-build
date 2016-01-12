package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class CommonFieldsHaveNoDescriptionsSpec extends FunSpec with Matchers {

  val linter = linters.CommonFieldsHaveNoDescriptions

  def buildService(
    name: String = "user",
    fields: Seq[String]
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(name, fields.map { name =>
          Services.buildField(name, description = Some("test"))
        })
      )
    )
  }

  it("no-op w/out common fields") {
    linter.validate(buildService(fields = Seq("foo", "bar"))) should be(Nil)
  }

  it("no-op w/out common fields for a form") {
    linters.CommonFieldsHaveNoDescriptions.NamesWithNoDescriptions.foreach { name =>
      linter.validate(
        buildService(name = "user_form", fields = Seq(name))
      ) should be(Nil)
    }
  }

  it("Common fields cannot have descriptions") {
    linters.CommonFieldsHaveNoDescriptions.NamesWithNoDescriptions.foreach { name =>
      linter.validate(
        buildService(fields = Seq(name))
      ) should be(
        Seq(s"Model user Field[$name]: Must not have a description")
      )
    }
  }

}
