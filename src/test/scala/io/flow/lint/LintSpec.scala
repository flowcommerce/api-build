package io.flow.lint

import org.scalatest.{FunSpec, Matchers}

class LintSpec extends FunSpec with Matchers with Helpers {

  it("temporary debugging") {
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

  it("healthcheck validates method") {
    Lint(
      ServiceBuilder().addResource(
        healthcheckResourceTemplate.format("POST", "/_internal_/healthcheck", 200, "io.flow.common.v0.models.healthcheck")
      ).service
    ).validate should be(Seq("healthchecks: Missing GET /_internal_/healthcheck"))
  }

  it("healthcheck validates path") {
    Lint(
      ServiceBuilder().addResource(
        healthcheckResourceTemplate.format("GET", "/healthcheck", 200, "io.flow.common.v0.models.healthcheck")
      ).service
    ).validate should be(Seq("healthchecks: Missing GET /_internal_/healthcheck"))
  }

  it("healthcheck validates response code") {
    Lint(
      ServiceBuilder().addResource(
        healthcheckResourceTemplate.format("GET", "/_internal_/healthcheck", 201, "io.flow.common.v0.models.healthcheck")
      ).service
    ).validate should be(
      Seq("healthchecks GET /_internal_/healthcheck: reponse must return HTTP 200 and not HTTP 201")
    )
  }

  it("healthcheck validates response type") {
    Lint(
      ServiceBuilder().addResource(
        healthcheckResourceTemplate.format("GET", "/_internal_/healthcheck", 200, "string")
      ).service
    ).validate should be(
      Seq("healthchecks GET /_internal_/healthcheck: response must be of type io.flow.common.v0.models.healthcheck and not string")
    )
  }

}
