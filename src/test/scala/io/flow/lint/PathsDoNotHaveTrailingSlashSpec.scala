package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class PathsDoNotHaveTrailingSlash extends FunSpec with Matchers {

  private[this] val linter = linters.PathsDoNotHaveTrailingSlash

  it("good path") {
    linter.validate(
      Services.buildServiceByPath("/:organization")
    ) should be(Nil)
    linter.validate(
      Services.buildServiceByPath("/:organization/users")
    ) should be(Nil)
  }

  it("bad path") {
    linter.validate(
      Services.buildServiceByPath("/:organization/")
    ) should be(
      Seq(
        "Resource organizations GET /:organization/: Path cannot end with '/'"
      )
    )
  }
}
