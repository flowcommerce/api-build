package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class LowerCasePathsSpec extends FunSpec with Matchers {

  val linter = linters.LowerCasePaths

  def buildService(
    path: String
  ): Service = {
    Services.Base.copy(
      resources = Seq(
        Resource(
          `type` = "organization",
          plural = "organizations",
          operations = Seq(
            Operation(
              method = Method.Get,
              path = path,
              parameters = Nil,
              responses = Seq(Services.buildResponse(`type` = "unit"))
            )
          )
        )
      )
    )
  }

  it("lower case paths are good") {
    linter.validate(
      buildService("/organizations/tiers/:tier_id")
    ) should be(Nil)
  }

  it("Flags upper case paths") {
    linter.validate(
      buildService("/organizations/tiers/:tierId")
    ) should be(
      Seq(
        "Resource organizations GET /organizations/tiers/:tierId: Path must be all lower case"
      )
    )
  }
}
