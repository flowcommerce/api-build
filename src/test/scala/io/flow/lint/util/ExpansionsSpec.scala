package io.flow.lint.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ExpansionsSpec extends AnyFunSpec with Matchers {

  it("fromFieldTypes") {
    Expansions.fromFieldTypes(Nil) should be(Nil)
    Expansions.fromFieldTypes(Seq("string")) should be(Nil)
    Expansions.fromFieldTypes(Seq("user")) should be(Nil)
    Expansions.fromFieldTypes(Seq("expandable_user")) should be(Seq("user"))
    Expansions.fromFieldTypes(Seq("string", "user", "expandable_user")) should be(Seq("user"))
  }

}
