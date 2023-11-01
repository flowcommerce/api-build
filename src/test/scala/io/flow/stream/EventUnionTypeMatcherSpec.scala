package io.flow.stream

import io.apibuilder.spec.v0.models.Field
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EventUnionTypeMatcherSpec extends AnyFunSpec with Matchers {
  it("matches field name, field type and union type name the same") {
    val field = Field(name = "organization", `type` = "organization", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "organization") shouldBe true
  }

  it("field type with namespace") {
    val field = Field(name = "organization", `type` = "io.flow.blah.v0.organization", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "organization") shouldBe true
  }

  it("field name short of union type") {
    val field = Field(name = "organization", `type` = "foo_organization_bar", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "foo_organization_bar") shouldBe true
  }

  it("field type short of union type") {
    val field = Field(name = "foo_organization_bar", `type` = "organization", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "foo_organization_bar") shouldBe true
  }

  it("field type short of union type with namespace") {
    val field = Field(name = "foo_organization_bar", `type` = "io.flow.blah.v0.organization", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "foo_organization_bar") shouldBe true
  }

  it("field name and type short of union type with namespace") {
    val field = Field(name = "organization", `type` = "io.flow.blah.v0.organization", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "foo_organization_bar") shouldBe true
  }

  it("field name short and type long of union type with namespace") {
    val field = Field(name = "organization", `type` = "io.flow.blah.v0.foo_organization_bar", required = true)
    EventUnionTypeMatcher.matchFieldToPayloadType(field, "organization") shouldBe true
  }
}
