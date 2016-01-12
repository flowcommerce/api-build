package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class ExpandableUnionsAreConsistentSpec extends FunSpec with Matchers {

  val linter = linters.ExpandableUnionsAreConsistent

  def buildService(types: Seq[String]): Service = {
    Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = "expandable_user",
          discriminator = Some("discriminator"),
          types = types.map { t =>
            Services.buildUnionType(t)
          }
        )
      )
    )
  }

  it("with no types") {
    linter.validate(buildService(Nil)) should be(
      Seq("Union expandable_user: Types for this expandable union must be user_reference, user")
    )
  }

  it("with single types") {
    linter.validate(buildService(Seq("user_reference"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user_reference, user and not user_reference")
    )

    linter.validate(buildService(Seq("user"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user_reference, user and not user")
    )
  }

  it("with valid types") {
    linter.validate(buildService(Seq("user_reference", "user"))) should be(Nil)
  }

  it("with valid types in invalid order") {
    linter.validate(buildService(Seq("user", "user_reference"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user_reference, user and not user, user_reference")
    )
  }

  it("with invalid types") {
    linter.validate(buildService(Seq("foo", "bar"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user_reference, user and not foo, bar")
    )
  }


}
