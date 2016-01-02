package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._

case class ServiceBuilder(
  models: Seq[String] = Nil,
  resources: Seq[String] = Nil
) {

  def addModel(model: String): ServiceBuilder = {
    ServiceBuilder(
      models = this.models ++ Seq(model),
      resources = resources
    )
  }

  def addResource(resource: String): ServiceBuilder = {
    ServiceBuilder(
      models = this.models,
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
    "models": [${models.mkString(",\n")}]
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
