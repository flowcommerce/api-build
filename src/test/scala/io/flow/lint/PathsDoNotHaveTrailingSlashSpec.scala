package io.flow.lint

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PathsDoNotHaveTrailingSlash extends AnyFunSpec with Matchers {

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
