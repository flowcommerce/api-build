package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class LowerCasePathsSpec extends FunSpec with Matchers {

  private[this] val linter = linters.LowerCasePaths

  it("lower case paths are good") {
    linter.validate(
      Services.buildServiceByPath("/organizations/tiers/:tier_id")
    ) should be(Nil)
  }

  it("Flags upper case paths") {
    linter.validate(
      Services.buildServiceByPath("/organizations/tiers/:tierId")
    ) should be(
      Seq(
        "Resource organizations GET /organizations/tiers/:tierId: Path must be all lower case"
      )
    )
  }
}
