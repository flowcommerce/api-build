package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AllAttributesAreWellKnownSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.AllAttributesAreWellKnown

  private[this] def build(names: Seq[String]): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          name = "id",
          attributes = names.map { name =>
            Services.buildAttribute(name = name)
          }
        )
      )
    )
  }

  private[this] val ErrorMsg =
    "Service contains an unknown attribute named 'auth' - remove this attribute or add to AllAttributesAreWellKnown.KnownAttributeNames in the api-build project (https://github.com/flowcommerce/api-build)"

  it("unsupported attribute") {
    linter.validate(
      build(Seq("auth"))
    ) shouldBe Seq(ErrorMsg)
  }

  it("unsupported attribute reported at most once") {
    linter.validate(
      build(Seq("auth", "auth"))
    ) shouldBe Seq(ErrorMsg)
  }

  it("other attribute are accepted") {
    def test(name: String) = linter.validate(build(Seq(name)))

    test("api-build") shouldBe Nil
    test("graphql") shouldBe Nil
    test("linter") shouldBe Nil
    test("non-crud") shouldBe Nil
    test("sort") shouldBe Nil
  }

}
