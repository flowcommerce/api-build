package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.{Application, Downloader}
import io.flow.registry.v0.{Client => RegistryClient}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import Text._

case class Controller() extends io.flow.build.Controller {

  /**
    * Whitelist of applications in the 'api' repo that do not exist in registry
    */
  private[this] val ExcludeWhiteList = Seq("common", "delivery_window")

  override val name = "Proxy"
  override val command = "proxy"

  def run(
    downloader: Downloader,
    allServices: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    val services = allServices.
      filter { s => !s.resources.isEmpty }.
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

    println(s"VERSION: $version")

    val registryClient = new RegistryClient()

    try {
      build(services, version, "production") { service =>
        s"https://${service.name.toLowerCase}.api.flow.io"
      }

      build(services, version, "development") { service =>
        val app = Await.result(
          getFromRegistry(registryClient, service.name),
          Duration(3, "seconds")
        ).getOrElse {
          sys.error(s"Could not find application named[${service.name}] in Registry at[${registryClient.baseUrl}]. Either add the service to the registry or, if it should never be part of api.flow.io, add to the proxy whitelist in api-build:src/main/scala/io/flow/proxy/Controller.scala")
        }

        val port = app.ports.headOption.getOrElse {
          sys.error(s"Application named[${service.name}] does not have any ports in Registry at[${registryClient.baseUrl}]")
        }.external

        s"http://localhost:$port"
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  private[this] def getFromRegistry(
    client: RegistryClient, name: String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Future[Option[io.flow.registry.v0.models.Application]] = {
    client.applications.getById(name).map { app =>
      Some(app)
    }.recover {
      case io.flow.registry.v0.errors.UnitResponse(404) => {
        None
      }

      case ex: Throwable => {
        sys.error(s"Error fetching application[$name] from registry at[${client.baseUrl}]; $ex")
      }
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
