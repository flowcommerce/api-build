package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class HealthcheckSpec extends FunSpec with Matchers {

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
    Lint(Services.Base).validate should be(
      Seq("Missing resource: healthchecks")
    )
  }

  it("healthcheck service is valid") {
    Lint(Services.withHealthcheck(Services.Base)).validate should be(Nil)
  }

  it("healthcheck validates method") {
    Lint(
      Services.Base.copy(
        resources = Seq(
          Services.buildSimpleResource(
            `type` = "io.flow.common.v0.models.healthcheck",
            plural = "healthchecks",
            method = Method.Post,
            path = "/_internal_/healthcheck",
            responseCode = 200,
            responseType = "io.flow.common.v0.models.healthcheck"
          )
        )
      )
    ).validate should be(Seq("healthchecks: Missing GET /_internal_/healthcheck"))
  }

  it("healthcheck validates path") {
    Lint(
      Services.Base.copy(
        resources = Seq(
          Services.buildSimpleResource(
            `type` = "io.flow.common.v0.models.healthcheck",
            plural = "healthchecks",
            method = Method.Get,
            path = "/healthcheck",
            responseCode = 200,
            responseType = "io.flow.common.v0.models.healthcheck"
          )
        )
      )
    ).validate should be(Seq("healthchecks: Missing GET /_internal_/healthcheck"))
  }

  it("healthcheck validates response code") {
    Lint(
      Services.Base.copy(
        resources = Seq(
          Services.buildSimpleResource(
            `type` = "io.flow.common.v0.models.healthcheck",
            plural = "healthchecks",
            method = Method.Get,
            path = "/_internal_/healthcheck",
            responseCode = 201,
            responseType = "io.flow.common.v0.models.healthcheck"
          )
        )
      )
    ).validate should be(
      Seq("healthchecks GET /_internal_/healthcheck: reponse must return HTTP 200 and not HTTP 201")
    )
  }

  it("healthcheck validates response type") {
    Lint(
      Services.Base.copy(
        resources = Seq(
          Services.buildSimpleResource(
            `type` = "io.flow.common.v0.models.healthcheck",
            plural = "healthchecks",
            method = Method.Get,
            path = "/_internal_/healthcheck",
            responseCode = 200,
            responseType = "string"
          )
        )
      )
    ).validate should be(
      Seq("healthchecks GET /_internal_/healthcheck: response must be of type io.flow.common.v0.models.healthcheck and not string")
    )
  }

}
