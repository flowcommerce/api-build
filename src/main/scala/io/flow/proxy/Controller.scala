package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.{Application, Downloader}
import io.flow.registry.v0.{Client => RegistryClient}
import Text._

case class Controller() extends io.flow.build.Controller {

  /**
    * Whitelist of applications in the 'api' repo that do not exist in registry
    */
  private[this] val ExcludeWhiteList = Seq("common", "delivery_window")

  /**
    * This is the hostname of the services when running in docker on
    * our development machines.
    */
  private[this] val DockerHostname = "172.17.0.1"

  private[this] val DevelopmentHostname = "localhost"

  override val name = "Proxy"
  override val command = "proxy"

  def run(
    downloader: Downloader,
    allServices: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    val services = allServices.
      filter { s => s.resources.nonEmpty }.
      filter { s => !ExcludeWhiteList.contains(s.name) }

    println("Building proxy from: " + services.map(_.name).mkString(", "))

    val version = downloader.service(Application("flow", "api", "latest")) match {
      case Left(error) => {
        sys.error(s"Failed to download 'api' service from apidoc: $error")
      }
      case Right(svc) => {
        svc.version
      }
    }

    val registryClient = new RegistryClient()
    try {
      build(services, version, "production") { service =>
        s"https://${service.name.toLowerCase}.api.flow.io"
      }

      val cache = RegistryApplicationCache(registryClient)
      build(services, version, "development") { service =>
        s"http://$DevelopmentHostname:${cache.externalPort(service.name.toLowerCase)}"
      }

      build(services, version, "workstation") { service =>
        s"http://$DockerHostname:${cache.externalPort(service.name.toLowerCase)}"
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  private[this] def build(
    services: Seq[Service],
    version: String,
    env: String
  ) (
    hostProvider: Service => String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
  
    val servicesYaml = services.map { service =>
      val host = hostProvider(service)
      ProxyBuilder(service, host).yaml()
    }.mkString("\n")

    val all = s"""version: $version
services:
${servicesYaml.indent(2)}
"""

    val path = s"/tmp/api-proxy.$env.config"
    writeToFile(path, all)
    println(s" - $env: $path")
  }

  private[this] def writeToFile(path: String, contents: String) {
    import java.io.{BufferedWriter, File, FileWriter}

    val bw = new BufferedWriter(new FileWriter(new File(path)))
    try {
      bw.write(contents)
    } finally {
      bw.close()
    }
  }
  
}
