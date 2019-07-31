package io.flow.proxy

import io.flow.helpers.ServiceHostHelpers
import org.scalatest.{FunSpec, Matchers}

class ApiBuildAttributesSpec extends FunSpec with Matchers
  with ServiceHostHelpers
{

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