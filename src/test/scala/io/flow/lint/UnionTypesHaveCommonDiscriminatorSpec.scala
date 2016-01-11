package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class UnionTypesHaveCommonDiscriminatorSpec extends FunSpec with Matchers {

  val linter = linters.UnionTypesHaveCommonDiscriminator

  def buildService(discriminator: Option[String]): Service = {
    Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = "expandable_user",
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
    linter.validate(buildService(None)) should be (
      Seq("Union expandable_user: Must have a discriminator with value 'discriminator'")
    )
  }

  it("with invalid discriminator") {
    linter.validate(buildService(Some("foo"))) should be (
      Seq("Union expandable_user: Discriminator must have value 'discriminator' and not foo")
    )
  }

  it("with valid discriminator") {
    linter.validate(buildService(Some("discriminator"))) should be (Nil)
  }

}
