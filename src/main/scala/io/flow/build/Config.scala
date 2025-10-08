package io.flow.build

import io.flow.{lint, oneapi, proxy, stream}
import scopt.OptionParser

import java.nio.file.{Files, Path}

case class Config(
  buildType: BuildType = BuildType.Api,
  protocol: String = "https",
  domain: String = "api.flow.io",
  productionConfig: Option[java.nio.file.Path] = None,
  buildCommand: String = "all",
  apis: Seq[String] = Seq(),
  output: java.nio.file.Path = java.nio.file.Paths.get("/tmp"),
)

object Config {
  def parseArgs(args: Array[String]): Option[Config] = makeParser().parse(args, Config())

  private implicit val BuildTypeRead: scopt.Read[BuildType] =
    scopt.Read.reads(s => BuildType.fromString(s).getOrElse(sys.error(s"Unknown BuildType '$s'")))

  private def makeParser(): OptionParser[Config] = new scopt.OptionParser[Config]("api-build") {
    override def showUsageOnError: Option[Boolean] = Some(true)

    arg[BuildType]("<build type>")
      .action((bt, c) => c.copy(buildType = bt))
      .text("One of: " + BuildType.all.map(_.toString).mkString(", "))
      .required()

    arg[String]("<build command>")
      .action((cmd, c) => c.copy(buildCommand = cmd))
      .text("One of: " + controllers(BuildType.Api).map(_.command).mkString("all, ", ", ", ""))
      .required()

    opt[Unit]("http-only")
      .text("If specified, results in http being used as the protocol in host names (default is 'https')")
      .action((_, c) => c.copy(protocol = "http"))

    opt[String]('d', "domain")
      .text("Domain to use when constructing the service subdomain (default is 'api.flow.io')")
      .action((d, c) => c.copy(domain = d))

    opt[Path]("production-config")
      .text("Optional yaml file to provide host configuration for servers (production only)")
      .validate { path =>
        if (!Files.exists(path)) failure(s"Production config file does not exist: '$path'")
        else if (!Files.isRegularFile(path)) failure(s"Production config path is not a file: '$path'")
        else success
      }
      .action((path, c) => c.copy(productionConfig = Some(path)))
    opt[Path]('o', "output")
      .text("Where to write output files (default is '/tmp')")
      .validate { path =>
        if (!Files.exists(path)) failure(s"Path does not exist: $path")
        else if (!Files.isDirectory(path)) failure(s"Path is not a directory: $path")
        else success
      }
      .action((p, c) => c.copy(output = p))

    arg[String]("<flow/experience>...")
      .text("API specs from APIBuilder")
      .action((api, c) => c.copy(apis = c.apis :+ api))
      .unbounded()
      .optional()
      .validate(api =>
        Application.parse(api) match {
          case Some(_) => success
          case None => failure(s"Could not parse application[$api]")
        },
      )

    help("help")

    checkConfig(c => {
      val selected = if (c.buildCommand == "all") {
        controllers(c.buildType)
      } else {
        controllers(c.buildType).filter(_.command == c.buildCommand)
      }
      selected.toList match {
        case Nil => {
          failure(
            s"Invalid command[${c.buildCommand}] for build type[${c.buildType}]. " +
              s"Must be one of: all, " + controllers(c.buildType).map(_.command).mkString(", "),
          )
        }
        case _ => {
          success
        }
      }
    })
  }

  private def controllers(buildType: BuildType): Seq[Controller] = {
    val all = scala.collection.mutable.ListBuffer[Controller]()
    all.append(lint.Controller())
    all.append(stream.Controller())
    if (buildType.oneApi) {
      all.append(oneapi.Controller())
    }
    if (buildType.proxy) {
      all.append(proxy.Controller())
    }
    all.toSeq
  }
}
