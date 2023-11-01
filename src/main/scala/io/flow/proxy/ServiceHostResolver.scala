package io.flow.proxy

import io.apibuilder.spec.v0.models.Service

case class ServiceHostResolver(services: Seq[Service]) {

  private[this] val apiBuildAttributes = ApiBuildAttributes(services)

  def host(serviceName: String): String = {
    apiBuildAttributes.host(serviceName).getOrElse {
      val formattedName = Text.stripSuffix(
        Text.stripSuffix(serviceName.toLowerCase, "-internal-event"),
        "-internal"
      )
      apiBuildAttributes.host(formattedName).getOrElse {
        formattedName
      }
    }
  }
}
