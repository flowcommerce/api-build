package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.registry.v0.{Client => RegistryClient}
import io.flow.registry.v0.models.Application
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import Text._

case class Controller() extends io.flow.build.Controller {

  override val name = "Proxy"
  override val command = "proxy"

  def run(
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    println("Building proxy from: " + services.map(_.name).mkString(", "))

    val registryClient = new RegistryClient()

    try {
      val version = "0.0.40" // todo

      build(services, version, "production") { service =>
        service.baseUrl.getOrElse {
          s"https://${service.name.toLowerCase}.api.flow.io"
        }
      }

      build(services, version, "development") { service =>
        Await.result(
          getFromRegistry(registryClient, service.name),
          Duration(3, "seconds")
        ) match {
          case None => {
            sys.error(s"Could not find application named[${service.name}] in Registry at[${registryClient.baseUrl}]")
          }

          case Some(app) => {
            println(s"APP: $app")
            sys.error("TODO")
          }
        }
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  private[this] def getFromRegistry(
    client: RegistryClient, name: String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Future[Option[Application]] = {
    client.applications.getById(name).map { app =>
      Some(app)
    }.recover {
      case io.flow.registry.v0.errors.UnitResponse(404) => {
        None
      }

      case io.flow.registry.v0.errors.UnitResponse(401) => {
        // TODO: Remove
        Some(
          Application(
            id = name,
            ports = Seq(
              Port(
                service = io.flow.registry.v0.models.ServiceReference("play"),
                external = 9000,
                external = 6081
              )
            ),
            dependencies = Nil
          )
        )
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
