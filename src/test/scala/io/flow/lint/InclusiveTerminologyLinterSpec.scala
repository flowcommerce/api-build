package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InclusiveTerminologyLinterSpec extends AnyFunSpec with Matchers {
  import Services._

  private[this] val linter = linters.InclusiveTerminologyLinter

  def test(
    headers: Seq[Header] = Nil,
    enums: Seq[Enum] = Nil,
    interfaces: Seq[Interface] = Nil,
    unions: Seq[Union] = Nil,
    models: Seq[Model] = Nil,
    resources: Seq[Resource] = Nil,
  ): Seq[String] = {
    linter.validate(
      Base.copy(
        headers = headers,
        enums = enums,
        interfaces = interfaces,
        unions = unions,
        models = models,
        resources = resources,
      )
    )
  }

  it("interface") {
    test(interfaces = Seq(
      buildInterface(name = "blacklist"))
    ) should be(
      Seq("Interface blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("interface.field") {
    test(interfaces = Seq(
      buildInterface(name = "example", fields = Seq(buildField("blacklist"))))
    ) should be(
      Seq("Interface example Field[blacklist]: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("model") {
    test(models = Seq(
      buildModel(name = "blacklist"))
    ) should be(
      Seq("Model blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }


  it("model.field") {
    test(models = Seq(
      buildModel(name = "example", fields = Seq(buildField("blacklist"))))
    ) should be(
      Seq("Model example Field[blacklist]: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("header") {
    test(headers = Seq(
      buildHeader(name = "blacklist"))
    ) should be(
      Seq("Header blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("enum") {
    test(enums = Seq(
      buildEnum(name = "blacklist")
    )) should be(
      Seq("Enum blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("enum.value") {
    test(enums = Seq(
      buildEnum(name = "example", values = Seq(buildEnumValue("blacklist")))
    )) should be(
      Seq("Enum example value blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("union") {
    test(unions = Seq(
      buildUnion(name = "blacklist")
    )) should be(
      Seq("Union blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("union.discriminator") {
    test(unions = Seq(
      buildUnion(name = "example", discriminator = Some("blacklist"))
    )) should be(
      Seq("Union example: discriminator: The term 'blacklist' must be replaced by 'denylist'")
    )
  }

  it("union.type") {
    test(unions = Seq(
      buildUnion(name = "example", types = Seq(buildUnionType("blacklist")))
    )) should be(
      Seq("Union example type blacklist: The term 'blacklist' must be replaced by 'denylist'")
    )
  }
}
