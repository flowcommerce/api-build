package io.flow.proxy

import io.apibuilder.spec.v0.models.{Attribute, Service}
import io.flow.lint.Services
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class ApiBuildAttributesSpec extends FunSpec with Matchers {

  private[this] def svc(name: String, host: Option[String] = None): Service = {
    Services.Base.copy(
      name = name,
      attributes = host.map { h =>
        Attribute(
          name = "api-build",
          value = Json.obj("host" -> h)
        )
      }.toSeq
    )
  }

  it("host with no attribute") {
    ApiBuildAttributes(
      Seq(svc("user"))
    ).host("user") should be(None)
  }

  it("host with attribute") {
    ApiBuildAttributes(
      Seq(svc("user"), svc("foo", Some("bar")))
    ).host("foo") should be(Some("bar"))
  }
}