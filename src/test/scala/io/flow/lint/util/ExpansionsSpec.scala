package io.flow.lint.util

import org.scalatest.{FunSpec, Matchers}

class ExpansionsSpec extends FunSpec with Matchers {

  it("fromFields") {
    Expansions.fromFields(Nil) should be(Nil)
    Expansions.fromFields(Seq("id")) should be(Nil)
    Expansions.fromFields(Seq("user")) should be(Nil)
    Expansions.fromFields(Seq("expandable_user")) should be(Seq("user"))
    Expansions.fromFields(Seq("id", "user", "expandable_user")) should be(Seq("user"))
  }

}
