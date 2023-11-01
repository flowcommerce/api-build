package io.flow.proxy

import io.flow.helpers.ServiceHostHelpers
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ApiBuildAttributesSpec extends AnyFunSpec with Matchers with ServiceHostHelpers {

  it("host with no attribute") {
    ApiBuildAttributes(
      Seq(serviceWithHost("user"))
    ).host("user") should be(None)
  }

  it("host with attribute") {
    ApiBuildAttributes(
      Seq(serviceWithHost("user"), serviceWithHost("foo", Some("bar")))
    ).host("foo") should be(Some("bar"))
  }
}
