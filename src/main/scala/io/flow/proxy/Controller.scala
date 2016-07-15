package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.{Application, Downloader}
import io.flow.registry.v0.{Client => RegistryClient}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import Text._
import org.yaml.snakeyaml.Yaml
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class Controller() extends io.flow.build.Controller {

  /**
    * Whitelist of applications in the 'api' repo that do not exist in registry
    */
  private[this] val ExcludeWhiteList = Seq("common", "delivery_window")
  private[this] val DockerGateway = "172.17.0.1"

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

    println(s"VERSION: $version")

    val registryClient = new RegistryClient()

    try {
      build(services, version, "production") { service =>
        s"https://${service.name.toLowerCase}.api.flow.io"
      }

      val serviceToPortMapping =
        Await.result(
          buildServiceToPortMappingFromRegistry(registryClient),
          Duration(3, "seconds")
        )

      build(services, version, "development") { service =>

        val svc = serviceToPortMapping.find(_.service == service).getOrElse {
          sys.error(s"Could not find application named[$service] in Registry at[${registryClient.baseUrl}]. Either add the service to the registry or, if it should never be part of api.flow.io, add to the proxy whitelist in api-build:src/main/scala/io/flow/proxy/Controller.scala")
        }

        s"http://localhost:${svc.port}"
      }

      build(services, version, "workstation") { service =>

        val svc = serviceToPortMapping.find(_.service == service).getOrElse {
          sys.error(s"Could not find application named[$service] in Registry at[${registryClient.baseUrl}]. Either add the service to the registry or, if it should never be part of api.flow.io, add to the proxy whitelist in api-build:src/main/scala/io/flow/proxy/Controller.scala")
        }

        s"http://$DockerGateway:${svc.port}"
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  case class ServicePort(
    service: String,
    port: String
  )

  /**
    * Hit Registry YAML endpoint to retrieve full list of applications (services, ports).
    * The entire list of applications is returned as Registry does the work of paginating all applications
    * into the YAML.
    * @param client
    * @param ec
    * @return
    */
  private[this] def buildServiceToPortMappingFromRegistry(
    client: RegistryClient
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Future[Seq[ServicePort]] = {
    client.applications.getYaml().map { registryYaml =>
      val yaml = new Yaml()

      Try {
        val y = Option(yaml.load(registryYaml))

        y match {
          case None => Map[String, Object]()
          case Some(data) => data.asInstanceOf[java.util.ArrayList[_]].asScala
        }

      } match {
        case Success(mapping) => {
          mapping.map(a => a match {
            case application: java.util.LinkedHashMap[String, Object] => {
              ServicePort(
                service = application.getOrDefault("id", sys.error(s"Could not find service name for application [$application]")).toString,
                port = application.get("ports").asInstanceOf[java.util.ArrayList[_]].asScala.map(p => p match {
                  case ports: java.util.LinkedHashMap[String, Object] => ports.getOrDefault("external", sys.error(s"Could not find external port in [$ports]")).toString
                  case _ => sys.error(s"Could not find ports in [$p]")
                }).head
              )
            }
            case Failure(ex) => sys.error(s"Could not find application in [$a].  Error(s): [${ex.getMessage}]")
          })
        }.toSeq
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
