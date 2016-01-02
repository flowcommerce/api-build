package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._

object Services {

  val Base = Service(
    apidoc = Apidoc(
      version = "0.9.50"
    ),
    name = "api-lint-test",
    organization = Organization(
      key = "flow"
    ),
    application = Application(
      key = "api-lint-test"
    ),
    namespace = "io.flow.api.lint.test",
    version = "0.0.1",
    info = Info()
  )

  lazy val HealthcheckResource = buildSimpleResource(
    `type` = "io.flow.common.v0.models.healthcheck",
    plural = "healthchecks",
    method = Method.Get,
    path = "/_internal_/healthcheck",
    responseCode = 200,
    responseType = "io.flow.common.v0.models.healthcheck"
  )

  def buildSimpleResource(
    `type`: String,
    plural: String,
    method: Method,
    path: String,
    responseCode: Int,
    responseType: String
  ): Resource = {
    Resource(
      `type` = `type`,
      plural = plural,
      operations = Seq(
        Operation(
          method = method,
          path = path,
          responses = Seq(
            Response(
              code = ResponseCodeInt(responseCode),
              `type` = responseType
            )
          )
        )
      )
    )
  }

  def withHealthcheck(service: Service): Service = {
    service.copy(
      resources = service.resources ++ Seq(HealthcheckResource)
    )
  }

}
