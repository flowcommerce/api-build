package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class UnionTypesHaveCommonDiscriminatorSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.UnionTypesHaveCommonDiscriminator

  def buildService(
    typeName: String,
    discriminator: Option[String]
  ): Service = {
    Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = typeName,
          discriminator = discriminator,
          types = Seq(
            Services.buildUnionType("string"),
            Services.buildUnionType("uuid")
          )
        )
      )
    )
  }

  it("with no discriminator") {
    linter.validate(buildService("expandable_user", None)) should be (
      Seq("Union expandable_user: Must have a discriminator with value one of ('discriminator', 'type', 'code')")
    )
  }

  it("with invalid discriminator") {
    linter.validate(buildService("expandable_user", Some("foo"))) should be (
      Seq("Union expandable_user: Discriminator must have value one of ('discriminator', 'type', 'code') and not 'foo'")
    )
  }

  it("with valid discriminator") {
    linter.validate(buildService("expandable_user", Some("discriminator"))) should be (Nil)
  }

  it("with valid discriminator - type") {
    linter.validate(buildService("expandable_user", Some("type"))) should be (Nil)
  }

  it("with valid discriminator for localized_price hack") {
    linter.validate(buildService("localized_price", Some("key"))) should be (Nil)
  }

  it("union types that end in _error must have a discriminator named 'code'") {
    linter.validate(buildService("user_error", None)) should be (
      Seq("Union user_error: Must have a discriminator with value one of ('code')")
    )
  }

  it("union types that end in _error with invalid discriminator") {
    linter.validate(buildService("user_error", Some("foo"))) should be (
      Seq("Union user_error: Discriminator must have value one of ('code') and not 'foo'")
    )
  }

  it("union types that end in _error with valid discriminator") {
    linter.validate(buildService("user_error", Some("code"))) should be (Nil)
  }

}
