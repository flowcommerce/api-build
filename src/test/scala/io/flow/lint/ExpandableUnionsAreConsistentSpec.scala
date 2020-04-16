package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ExpandableUnionsAreConsistentSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.ExpandableUnionsAreConsistent

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
      Seq("Union expandable_user: must contain the following types: user, user_reference")
    )
  }

  it("with single types") {
    linter.validate(buildService(Seq("user_reference"))) should be(
      Seq("Union expandable_user: must contain a type named 'user'")
    )

    linter.validate(buildService(Seq("user"))) should be(
      Seq("Union expandable_user: must contain a type named 'user_reference'")
    )
  }

  it("with valid types") {
    linter.validate(buildService(Seq("user", "user_reference"))) should be(Nil)
  }

  it("with valid types in invalid order") {
    linter.validate(buildService(Seq("user_reference", "user"))) should be(
      Seq("Union expandable_user: types must be in the following order: user, user_reference")
    )
  }

  it("with invalid types") {
    linter.validate(buildService(Seq("foo", "bar"))) should be(
      Seq("Union expandable_user: must contain the following types: user, user_reference")
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
      Seq("Union expandable_payment: must contain a type named 'payment_paypal'")
    )
  }

  it("allows additional types") {
    val s = Services.Base.copy(
      unions = Seq(
        Services.buildUnion(
          name = "expandable_card",
          discriminator = Some("discriminator"),
          types = Seq(
            Services.buildUnionType("card"),
            Services.buildUnionType("card_reference"),
            Services.buildUnionType("card_summary")
          )
        )
      )
    )

    linter.validate(s) should be(Nil)
  }
}
