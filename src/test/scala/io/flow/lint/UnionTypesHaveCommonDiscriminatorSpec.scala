package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class UnionTypesHaveCommonDiscriminatorSpec extends FunSpec with Matchers {

  val linter = linters.UnionTypesHaveCommonDiscriminator

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
      Seq("Union expandable_user: Must have a discriminator with value 'discriminator'")
    )
  }

  it("with invalid discriminator") {
    linter.validate(buildService("expandable_user", Some("foo"))) should be (
      Seq("Union expandable_user: Discriminator must have value 'discriminator' and not 'foo'")
    )
  }

  it("with valid discriminator") {
    linter.validate(buildService("expandable_user", Some("discriminator"))) should be (Nil)
  }

  it("with valid discriminator for error types") {
    linter.validate(buildService("example_error", Some("discriminator"))) should be (
      Seq("Union example_error: Discriminator must have value 'code' and not 'discriminator'")
    )

    linter.validate(buildService("example_error", Some("code"))) should be (Nil)
  }

}
