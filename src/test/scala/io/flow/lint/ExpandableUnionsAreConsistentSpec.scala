package io.flow.lint

import io.apibuilder.spec.v0.models._
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
      Seq("Union expandable_user: Types for this expandable union must be user, user_reference")
    )
  }

  it("with single types") {
    linter.validate(buildService(Seq("user_reference"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user, user_reference and not user_reference")
    )

    linter.validate(buildService(Seq("user"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user, user_reference and not user")
    )
  }

  it("with valid types") {
    linter.validate(buildService(Seq("user", "user_reference"))) should be(Nil)
  }

  it("with valid types in invalid order") {
    linter.validate(buildService(Seq("user_reference", "user"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user, user_reference and not user_reference, user")
    )
  }

  it("with invalid types") {
    linter.validate(buildService(Seq("foo", "bar"))) should be(
      Seq("Union expandable_user: Types for this expandable union must be user, user_reference and not foo, bar")
    )
  }

  it("with valid types when the type itself is a union") {
    val s = Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = "expandable_payment",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("payment_paypal"),
            Services.buildUnionType("payment_reference")
          )
        ),
        Services.buildUnion(
          name = "payment",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("payment_paypal")
          )
        )
      )
    )

    linter.validate(s) should be(Nil)
  }

  it("with invalid types when the type itself is a union") {
    val s = Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = "expandable_payment",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("other"),
            Services.buildUnionType("payment_reference")
          )
        ),
        Services.buildUnion(
          name = "payment",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("payment_paypal")
          )
        )
      )
    )

    linter.validate(s) should be(
      Seq("Union expandable_payment: Types for this expandable union must be payment_paypal, payment_reference and not other, payment_reference")
    )
  }

}
