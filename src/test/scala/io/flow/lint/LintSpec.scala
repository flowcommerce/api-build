package io.flow.lint

import org.scalatest.{FunSpec, Matchers}

class LintSpec extends FunSpec with Matchers {

  val healthcheck = ServiceBuilder().addResource("""
    {
      "type": "io.flow.common.v0.models.healthcheck",
      "plural": "healthchecks",
      "operations": [
        {
          "method": "GET",
          "path": "/_internal_/healthcheck",
          "parameters": [],
          "responses": [
            {
              "code": {
                "integer": {
                  "value": 200
                }
              },
              "type": "io.flow.common.v0.models.healthcheck"
            }
          ]
        }
      ]
    }
  """)

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

  it("healthcheck service is valid") {
    Lint(healthcheck.service).validate should be(Nil)
  }

}
