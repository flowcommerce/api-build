package io.flow.helpers

import io.apibuilder.spec.v0.models.{Attribute, Service}
import io.flow.lint.Services
import play.api.libs.json.Json

trait ServiceHostHelpers {

  def serviceWithHost(name: String, host: Option[String] = None): Service = {
    Services.Base.copy(name = name,
      attributes = host.map { h =>
        Attribute(
          name = "api-build",
          value = Json.obj("host" -> h)
        )
      }.toSeq
    )
  }

}