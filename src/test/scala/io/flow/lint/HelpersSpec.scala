package io.flow.lint.linters

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class HelpersSpec extends AnyFunSpec with Matchers {

  case object TestHelper extends Helpers

  it("isCountry") {
    TestHelper.isCountry("country") should be(true)
    TestHelper.isCountry("origin") should be(true)
    TestHelper.isCountry("id") should be(false)
  }

  it("isCurrency") {
    TestHelper.isCurrency("currency") should be(true)
    TestHelper.isCurrency("id") should be(false)
  }

  it("isLanguage") {
    TestHelper.isLanguage("language") should be(true)
    TestHelper.isLanguage("id") should be(false)
  }

}
