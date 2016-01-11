package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class HealthcheckSpec extends FunSpec with Matchers {

  val linter = linters.Healthcheck

  def buildService(
    `type`: String = "io.flow.common.v0.models.healthcheck",
    plural: String = "healthchecks",
    method: Method = Method.Get,
    path: String = "/_internal_/healthcheck",
    responseCode: Int = 200,
    responseType: String = "io.flow.common.v0.models.healthcheck"
  ): Service = {
    Services.Base.copy(
      resources = Seq(
        Services.buildSimpleResource(
          `type` = `type`,
          plural = plural,
          method = method,
          path = path,
          responseCode = responseCode,
          responseType = responseType
        )
      )
    )
  }

  it("does not require healthcheck if there are no resources") {
    linter.validate(Services.Base) should be(Nil)
  }

  it("requires healthcheck if there are resources") {
    linter.validate(buildService(`type` = "user", plural = "users", path = "/users/:id", responseType = "user")) should be(
      Seq("Missing resource: healthchecks")
    )
  }

  it("healthcheck service is valid") {
    linter.validate(Services.withHealthcheck(Services.Base)) should be(Nil)
  }

  it("healthcheck validates method") {
    linter.validate(buildService(method = Method.Post)) should be(
      Seq("Resource healthchecks: Missing GET /_internal_/healthcheck")
    )
  }

  it("healthcheck validates path") {
    linter.validate(buildService(path = "/foo")) should be(
      Seq("Resource healthchecks: Missing GET /_internal_/healthcheck")
    )
  }

  it("healthcheck validates response code") {
    linter.validate(buildService(responseCode = 201)) should be(
      Seq("Resource healthchecks GET /_internal_/healthcheck: reponse must return HTTP 200 and not HTTP 201")
    )
  }

  it("healthcheck validates response type") {
    linter.validate(buildService(responseType = "string")) should be(
      Seq("Resource healthchecks GET /_internal_/healthcheck: response must be of type io.flow.common.v0.models.healthcheck and not string")
    )
  }

}
