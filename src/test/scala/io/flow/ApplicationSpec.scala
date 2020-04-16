package io.flow.build

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ApplicationSpec extends AnyFunSpec with Matchers {

  it("parse valid strings") {
    Application.parse("flow/user") should be(Some(Application("flow", "user", "latest")))
    Application.parse("  flow/user  ") should be(Some(Application("flow", "user", "latest")))
    Application.parse("  flow / user  ") should be(Some(Application("flow", "user", "latest")))
    Application.parse("  flow / user:1.2.3  ") should be(Some(Application("flow", "user", "1.2.3")))
  }

  it("parse invalid strings") {
    Application.parse("flow") should be(None)
    Application.parse("   ") should be(None)
    Application.parse("flow/user/bar") should be(None)
  }

}
