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

  def buildModel(
    name: String,
    fields: Seq[Field] = Nil
  ): Model = {
    Model(
      name = name,
      plural = name + "s",
      fields = fields
    )
  }

  def buildField(
    name: String,
    `type`: String = "string",
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    default: Option[String] = None,
    required: Boolean = true,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    example: Option[String] = None
  ): Field = {
    Field(
      name = name,
      `type` = `type`,
      description = description,
      deprecation = deprecation,
      default = default,
      required = required,
      minimum = minimum,
      maximum = maximum,
      example = example
    )
  }

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
            buildResponse(responseCode, responseType)
          )
        )
      )
    )
  }

  def buildResponse(
    code: Int = 200,
    `type`: String
  ) = Response(
    code = ResponseCodeInt(code),
    `type` = `type`
  )

  def withHealthcheck(service: Service): Service = {
    service.copy(
      resources = service.resources ++ Seq(HealthcheckResource)
    )
  }

}
