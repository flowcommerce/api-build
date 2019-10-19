package io.flow.proxy

import io.apibuilder.spec.v0.models.Service
import io.flow.build.{Application, BuildType, Downloader}
import io.flow.registry.v0.{Client => RegistryClient}
import play.api.libs.json.Json

case class Controller() extends io.flow.build.Controller {

  /**
    * Whitelist of applications in the 'api' repo that do not exist in registry
    */
  private[this] val ExcludeWhiteList = Seq("common", "healthcheck", "usage", "gift-card")

  /**
    * This is the hostname of the services when running in docker on
    * our development machines.
    */
  private[this] val DockerHostname = "172.17.0.1"

  private[this] val DevelopmentHostname = "localhost"

  override val name = "Proxy"
  override val command = "proxy"

  def buildUserPermissionsFile(
    buildType: BuildType,
    services: Seq[Service]
  ): Unit = {
    val routes = services.flatMap(s=>s.resources.flatMap(r=>r.operations.flatMap{o=>
      o.attributes.find(_.name == "auth") match {
        case Some(a)=> {
          val ts = a.value \ "techniques"
          val rs = a.value \ "roles"
          ts.as[Seq[String]]
            .filterNot(_ == "user")
            .map(t=>(t, Map("method" -> o.method.toString, "path"-> o.path))) ++
          rs.asOpt[Seq[String]]
            .map(r=>r.map { t =>
              (t, Map("method" -> o.method.toString, "path" -> o.path))
            }).getOrElse(Nil)
        }
        case None => {
          Seq(("anonymous", Map("method" -> o.method.toString, "path"-> o.path)))
        }
      }
    }))
    val rs= routes.groupBy(_._1).map(r=> (r._1, Map("routes" -> r._2.map(_._2).distinct)))
    val m = Json.toJson(rs)

    val path = s"/tmp/${buildType}-authorization.json"
    writeToFile(path, Json.prettyPrint(m))
    println(s" - $path")
  }

  def run(
    buildType: BuildType,
    downloader: Downloader,
    allServices: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Unit = {
    val services = allServices.
      filter { s => s.resources.nonEmpty }.
      filterNot { s => ExcludeWhiteList.exists(ew => s.name.startsWith(ew)) }

    val serviceHostResolver = ServiceHostResolver(allServices)

    val version = downloader.service(Application("flow", buildType.toString, "latest")) match {
      case Left(error) => {
        sys.error(s"Failed to download '$buildType' service from apibuilder: $error")
      }
      case Right(svc) => {
        svc.version
      }
    }

    println("Building authorization from: " + services.map(_.name).mkString(", "))
    buildUserPermissionsFile(buildType, services)

    println("Building proxy from: " + services.map(_.name).mkString(", "))

    val registryClient = new RegistryClient()
    try {
      buildProxyFile(buildType, services, version, "production") { service =>
        s"https://${serviceHostResolver.host(service.name)}.api.flow.io"
      }

      val cache = RegistryApplicationCache(registryClient)

      def externalPort(service: Service): Long = cache.externalPort(
        registryName = serviceHostResolver.host(service.name),
        serviceName = service.name
      )

      buildProxyFile(buildType, services, version, "development") { service =>
        s"http://$DevelopmentHostname:${externalPort(service)}"
      }

      buildProxyFile(buildType, services, version, "workstation") { service =>
        s"http://$DockerHostname:${externalPort(service)}"
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  private[this] def buildProxyFile(
    buildType: BuildType,
    services: Seq[Service],
    version: String,
    env: String
  ) (
    hostProvider: Service => String
  ): Unit = {
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

  private[this] def writeToFile(path: String, contents: String): Unit = {
    import java.io.{BufferedWriter, File, FileWriter}

    val bw = new BufferedWriter(new FileWriter(new File(path)))
    try {
      bw.write(contents)
    } finally {
      bw.close()
    }
  }

}
