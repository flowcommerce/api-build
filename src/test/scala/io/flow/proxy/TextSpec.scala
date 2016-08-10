package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class TextSpec extends FunSpec with Matchers {

  it("shortenName") {
    Text.shortenName("") should be(None)
    Text.shortenName("currency") should be(None)
    Text.shortenName("currency-internal") should be(Some("currency"))
    Text.shortenName("currency-internal-spec") should be(Some("currency-internal"))
  }
  
}
