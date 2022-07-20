package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NoDeprecatedAttributesSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.NoDeprecatedAttributes

  private[this] def build(names: Seq[String]): Service = {
    Services.Base.copy(
      models = Seq(Services.buildModel(name = "id", attributes = names.map { name =>
        Services.buildAttribute(name = name)
      }))
    )
  }

  it("auth attribute is deprecated") {
    linter.validate(
      build(Seq("auth"))
    ) shouldBe Seq("Service contains a deprecated attribute named 'auth' - remove this attribute")
  }

  it("reports deprecation once") {
    linter.validate(
      build(Seq("auth", "auth"))
    ) shouldBe Seq("Service contains a deprecated attribute named 'auth' - remove this attribute")
  }

  it("other attribute are accepted") {
    linter.validate(build(Seq("other"))) shouldBe Nil
  }

}
