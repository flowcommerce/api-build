package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class CommonFieldsHaveNoDescriptionsSpec extends FunSpec with Matchers {

  val linter = linters.CommonFieldsHaveNoDescriptions

  def buildService(fields: Seq[String]): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel("user", fields.map { name =>
          Services.buildField(name, description = Some("test"))
        })
      )
    )
  }

  it("no-op w/out common fields") {
    linter.validate(buildService(Seq("foo", "bar"))) should be(Nil)
  }

  it("Common parameters cannot have descriptions") {
    linters.CommonFieldsHaveNoDescriptions.NamesWithNoDescriptions.foreach { name =>
      linter.validate(
        buildService(Seq(name))
      ) should be(
        Seq(s"Model user Field[$name]: Must not have a description")
      )
    }
  }

}
