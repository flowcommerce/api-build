package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._

import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._

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

case class ServiceBuilder(
  resources: Seq[String] = Nil
) {

  def addResource(resource: String): ServiceBuilder = {
    ServiceBuilder(
      resources = this.resources ++ Seq(resource)
    )
  }

  lazy val service = Json.parse(json).validate[Service] match {
    case e: JsError => sys.error("Failed to build service: " + e)
    case s: JsSuccess[Service] => s.get
  }

  lazy val json: String = buildJson(s"""
    "imports": [],
    "headers": [],
    "info": [],
    "enums": [],
    "unions": [],
    "resources": [${resources.mkString(",\n")}],
    "models": []
  """)

  def buildJson(json: String): String = {
    val specVersion = "1.0.0"
    val body = s"""
      "apidoc": { "version": "$specVersion" },
      "base_url": "http://localhost:9000",
      "name": "Api Lint Test",
      "organization": { "key": "io.flow" },
      "application": { "key": "lint-test" },
      "namespace": "io.flow.apilinttest.v0",
      "version": "1.0.0"
    """

    json.isEmpty match {
      case true => s"{ $body } "
      case false => s"{ $body, $json } "
    }
  }

}
