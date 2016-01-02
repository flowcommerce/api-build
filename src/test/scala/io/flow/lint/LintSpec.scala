package io.flow.lint

import org.scalatest.{FunSpec, Matchers}

class LintSpec extends FunSpec with Matchers {

  it("my test") {
    Lint.fromFile("/tmp/organization.json").validate() match {
      case Nil => println("valid")
      case errors => {
        println("1 or more errors:")
        errors.foreach { err =>
          println(s"  - $err")
        }
      }
    }
  }

  it("requires healthcheck") {
    Lint(ServiceBuilder().service).validate should be(
      Seq("Missing resource: healthchecks")
    )
  }

}
