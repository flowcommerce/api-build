package io.flow.proxy

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TextSpec extends AnyFunSpec with Matchers {

  it("stripSuffix") {
    Text.stripSuffix("", "-internal") should be("")
    Text.stripSuffix("currency", "-internal") should be("currency")
    Text.stripSuffix("currency-internal", "-internal") should be("currency")
    Text.stripSuffix("currency-internal-foo", "-internal") should be("currency-internal-foo")
  }
  
}
