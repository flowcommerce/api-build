package io.flow.lint

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LowerCasePathsSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.LowerCasePaths

  it("lower case paths are good") {
    linter.validate(
      Services.buildServiceByPath("/organizations/tiers/:tier_id"),
    ) should be(Nil)
  }

  it("Flags upper case paths") {
    linter.validate(
      Services.buildServiceByPath("/organizations/tiers/:tierId"),
    ) should be(
      Seq(
        "Resource organizations GET /organizations/tiers/:tierId: Path must be all lower case",
      ),
    )
  }
}
