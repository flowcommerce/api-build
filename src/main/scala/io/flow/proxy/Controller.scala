package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.{Application, BuildType, Downloader}
import io.flow.registry.v0.{Client => RegistryClient}
import Text._

case class Controller() extends io.flow.build.Controller {

  /**
    * Whitelist of applications in the 'api' repo that do not exist in registry
    */
  private[this] val ExcludeWhiteList = Seq("common", "delivery_window", "healthcheck")

  /**
    * This is the hostname of the services when running in docker on
    * our development machines.
    */
  private[this] val DockerHostname = "172.17.0.1"

  private[this] val DevelopmentHostname = "localhost"

  override val name = "Proxy"
  override val command = "proxy"

  def run(
    buildType: BuildType,
    downloader: Downloader,
    allServices: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    val services = allServices.
      filter { s => s.resources.nonEmpty }.
      filter { s => !ExcludeWhiteList.contains(s.name) }

    println("Building proxy from: " + services.map(_.name).mkString(", "))

    def serviceHost(name: String): String = {
      buildType match {
        case BuildType.Api => name.toLowerCase
        case BuildType.ApiEvent => name.toLowerCase
        case BuildType.ApiInternal => Text.stripSuffix(name.toLowerCase, "-internal")
        case BuildType.ApiInternalEvent => Text.stripSuffix(name.toLowerCase, "-internal-event")
        case BuildType.ApiPartner => Text.stripSuffix(name.toLowerCase, "-partner")
      }
    }
    
    val version = downloader.service(Application("flow", buildType.toString, "latest")) match {
      case Left(error) => {
        sys.error(s"Failed to download '$buildType' service from apidoc: $error")
      }
      case Right(svc) => {
        svc.version
      }
    }

    val registryClient = new RegistryClient()
    try {
      build(buildType, services, version, "production") { service =>
        s"https://${serviceHost(service.name)}.api.flow.io"
      }

      val cache = RegistryApplicationCache(registryClient)

      build(buildType, services, version, "development") { service =>
        s"http://$DevelopmentHostname:${cache.externalPort(serviceHost(service.name))}"
      }

      build(buildType, services, version, "workstation") { service =>
        s"http://$DockerHostname:${cache.externalPort(serviceHost(service.name))}"
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  private[this] def build(
    buildType: BuildType,
    services: Seq[Service],
    version: String,
    env: String
  ) (
    hostProvider: Service => String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    services.toList match {
      case Nil => {
        println(s" - $env: No services - skipping proxy file")
      }

      case _ => {
        val serversYaml = services.map { service =>
          Seq(
            s"- name: ${service.name}",
            s"  host: ${hostProvider(service)}"
          ).mkString("\n")
        }.mkString("\n")

        val operationsYaml = services.flatMap { service =>
          service.resources.flatMap(_.operations).map { op =>
            Seq(
              s"- method: ${op.method.toString.toUpperCase}",
              s"  path: ${op.path}",
              s"  server: ${service.name}"
            ).mkString("\n")
          }
        }.mkString("\n")

        val all = s"""version: $version

servers:
${serversYaml.indent(2)}

operations:
${operationsYaml.indent(2)}
"""

        val path = s"/tmp/${buildType}-proxy.$env.config"
        writeToFile(path, all)
        println(s" - $env: $path")
      }
    }
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
