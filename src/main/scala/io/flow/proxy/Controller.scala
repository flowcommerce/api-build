package io.flow.proxy

import io.apibuilder.spec.v0.models.Service
import io.flow.build.{Application, BuildConfig, BuildType, DownloadCache}
import io.flow.registry.v0.{Client => RegistryClient}
import play.api.libs.json.Json

import java.io.File
import java.nio.file.Path

case class Controller() extends io.flow.build.Controller {

  /** Allowlist of applications in the 'api' repo that do not exist in registry
    */
  private[this] val ExcludeAllowList = Seq("common", "healthcheck", "usage", "gift-card")

  /** This is the hostname of the services when running in docker on our development machines.
    */
  private[this] val DockerHostname = "172.17.0.1"

  private[this] val DevelopmentHostname = "localhost"

  override val name = "Proxy"
  override val command = "proxy"

  private def buildUserPermissionsFile(
    buildType: BuildType,
    output: Path,
    services: Seq[Service],
  ): Unit = {
    val routes = services.flatMap(s =>
      s.resources.flatMap(r =>
        r.operations.flatMap { o =>
          o.attributes.find(_.name == "auth") match {
            case Some(a) => {
              val ts = a.value \ "techniques"
              val rs = a.value \ "roles"
              ts.as[Seq[String]]
                .filterNot(_ == "user")
                .map(t => (t, Map("method" -> o.method.toString, "path" -> o.path))) ++
                rs.asOpt[Seq[String]]
                  .map(r =>
                    r.map { t =>
                      (t, Map("method" -> o.method.toString, "path" -> o.path))
                    },
                  )
                  .getOrElse(Nil)
            }
            case None => {
              Seq(("anonymous", Map("method" -> o.method.toString, "path" -> o.path)))
            }
          }
        },
      ),
    )
    val rs = routes.groupBy(_._1).map(r => (r._1, Map("routes" -> r._2.map(_._2).distinct)))
    val m = Json.toJson(rs)

    val path = output.resolve(s"${buildType}-authorization.json").toFile
    writeToFile(path, Json.prettyPrint(m))
    println(s" - $path")
  }

  def run(
    buildType: BuildType,
    buildConfig: BuildConfig,
    downloadCache: DownloadCache,
    allServices: Seq[Service],
  )(implicit
    ec: scala.concurrent.ExecutionContext,
  ): Unit = {
    val services = allServices.filter { s => s.resources.nonEmpty }.filterNot { s =>
      ExcludeAllowList.exists(ew => s.name.startsWith(ew))
    }

    val serviceHostResolver = ServiceHostResolver(allServices)

    val version = downloadCache.downloadService(Application.latest("flow", buildType.key)) match {
      case Left(error) => sys.error(s"Failed to download '$buildType' service from API Builder: $error")
      case Right(svc) => svc.version
    }

    println("Building authorization from: " + services.map(_.name).mkString(", "))
    buildUserPermissionsFile(buildType, buildConfig.output, services)

    println("Building proxy from: " + services.map(_.name).mkString(", "))

    val registryClient = new RegistryClient()
    try {
      buildProxyFile(buildType, buildConfig.output, services, version, "production") { service =>
        s"${buildConfig.protocol}://${serviceHostResolver.host(service.name)}.${buildConfig.domain}"
      }

      val cache = RegistryApplicationCache(registryClient)

      def externalPort(service: Service): Long = cache.externalPort(
        registryName = serviceHostResolver.host(service.name),
        serviceName = service.name,
      )

      buildProxyFile(buildType, buildConfig.output, services, version, "development") { service =>
        s"http://$DevelopmentHostname:${externalPort(service)}"
      }

      buildProxyFile(buildType, buildConfig.output, services, version, "workstation") { service =>
        s"http://$DockerHostname:${externalPort(service)}"
      }
    } finally {
      registryClient.closeAsyncHttpClient()
    }
  }

  private[this] def buildProxyFile(
    buildType: BuildType,
    output: Path,
    services: Seq[Service],
    version: String,
    env: String,
  )(
    hostProvider: Service => String,
  ): Unit = {
    services.toList match {
      case Nil => {
        println(s" - $env: No services - skipping proxy file")
      }

      case _ => {
        val serversYaml = services
          .map { service =>
            Seq(
              s"- name: ${service.name}",
              s"  host: ${hostProvider(service)}",
            ).mkString("\n")
          }
          .mkString("\n")

        val operationsYaml = services
          .flatMap { service =>
            service.resources.flatMap(_.operations).map { op =>
              Seq(
                s"- method: ${op.method.toString.toUpperCase}",
                s"  path: ${op.path}",
                s"  server: ${service.name}",
              ).mkString("\n")
            }
          }
          .mkString("\n")

        val all = s"""version: $version

servers:
${Text.indent(serversYaml, 2)}

operations:
${Text.indent(operationsYaml, 2)}
"""

        val path = output.resolve(s"${buildType}-proxy.$env.config").toFile
        writeToFile(path, all)
        println(s" - $env: $path")
      }
    }
  }

  private[this] def writeToFile(path: File, contents: String): Unit = {
    import java.io.{BufferedWriter, FileWriter}

    val bw = new BufferedWriter(new FileWriter(path))
    try {
      bw.write(contents)
    } finally {
      bw.close()
    }
  }

}
