package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service

case class ProxyBuilder(service: Service) {

  private[this] val host = service.baseUrl.getOrElse {
    s"https://${service.name.toLowerCase}.api.flow.io"
  }

  def yaml(): String = {
    val routes = service.resources.map { resource =>
      resource.operations.map { op =>
        Route(
          method = op.method.toString,
          path = op.path
        )
      }
    }.flatten

    Seq(
      s"${service.name}:",
      s"  - host: $host",
      s"  - operations:",
      routes.sorted.map { _.yaml }.mkString("    - ", "\n    - ", "")
    ).mkString("\n")
  }

}
