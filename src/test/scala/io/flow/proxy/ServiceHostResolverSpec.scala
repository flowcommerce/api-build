package io.flow.proxy

import io.flow.helpers.ServiceHostHelpers
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ServiceHostResolverSpec extends AnyFunSpec with Matchers with ServiceHostHelpers {

  it("host defaults to name of service") {
    val resolver = ServiceHostResolver(
      Seq(
        serviceWithHost("foo")
      )
    )
    resolver.host("foo") should be("foo")
  }

  it("host strips internal suffixes") {
    val resolver = ServiceHostResolver(
      Seq(
        serviceWithHost("user"),
        serviceWithHost("user-internal"),
        serviceWithHost("user-internal-event")
      )
    )
    resolver.host("user") should be("user")
    resolver.host("user-internal") should be("user")
    resolver.host("user-internal-event") should be("user")
  }

  it("respects attribute when specified") {
    val resolver = ServiceHostResolver(
      Seq(
        serviceWithHost("user", Some("foo")),
        serviceWithHost("user-internal", Some("bar")),
        serviceWithHost("user-internal-event", Some("baz"))
      )
    )
    resolver.host("user") should be("foo")
    resolver.host("user-internal") should be("bar")
    resolver.host("user-internal-event") should be("baz")
  }

  it("respects attribute 'host' on parent") {
    val resolver = ServiceHostResolver(
      Seq(
        serviceWithHost("user", Some("foo")),
        serviceWithHost("user-internal")
      )
    )
    resolver.host("user") should be("foo")
    resolver.host("user-internal") should be("foo")
  }

}
