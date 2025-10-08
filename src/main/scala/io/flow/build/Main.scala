package io.flow.build

import io.apibuilder.spec.v0.models.Service
import io.flow.{lint, oneapi, proxy, stream}

object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  private[this] val globalErrors = scala.collection.mutable.ListBuffer[String]()

  private[this] def controllers(buildType: BuildType): Seq[Controller] = {
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

  ApibuilderConfig.load() match {
    case Left(error) => {
      println(s"** Error loading apibuilder config: $error")
      System.exit(1)
    }
    case Right(profile) =>
      Config.parseArgs(args) match {
        case Some(config) =>
          val selected = if (config.buildCommand == "all") {
            controllers(config.buildType)
          } else {
            controllers(config.buildType).filter(_.command == config.buildCommand)
          }

          val allApplications: Seq[Application] = config.apis.flatMap { name =>
            Application.parse(name)
          }
          val serverConfigs: Seq[ServerConfig] = config.productionConfigOverride
            .map { path =>
              ServerConfig.parseFile(path) match {
                case Left(error) => sys.error(s"Failed to parse $path: '$error'")
                case Right(serverConfigs) => serverConfigs
              }
            }
            .getOrElse(Nil)

          val buildConfig = BuildConfig(
            protocol = config.protocol,
            domain = config.domain,
            productionServerConfigs = serverConfigs,
            output = config.output,
          )
          val dl = DownloadCache(Downloader(profile))
          dl.downloadServices(allApplications) match {
            case Left(errors) =>
              println(s"Errors downloading services:")
              errors.foreach { e => println(s" - $e") }
              System.exit(errors.length)

            case Right(services) =>
              run(config.buildType, buildConfig, dl, selected, services)
          }
        case None =>
        // error message already printed
      }
  }

  private[this] def run(
    buildType: BuildType,
    buildConfig: BuildConfig,
    downloadCache: DownloadCache,
    controllers: Seq[Controller],
    services: Seq[Service],
  ): Unit = {
    val errors = scala.collection.mutable.Map[String, Seq[String]]()
    if (globalErrors.nonEmpty) {
      errors += ("config" -> globalErrors.toSeq)
    }

    controllers.foreach { controller =>
      println("==================================================")
      println(s"${controller.name} Starting")
      println("==================================================")

      controller.run(buildType, buildConfig, downloadCache, services)
      controller.errors().foreach {
        case (key, errs) => {
          errors.get(key) match {
            case None => {
              errors.put(key, errs)
            }
            case Some(existing) => {
              errors.put(key, existing ++ errs)
            }
          }
        }
      }
    }

    println("")
    errors.size match {
      case 0 => println(s"SUMMARY: NO ERRORS")
      case 1 => println(s"SUMMARY: 1 ERROR")
      case n => println(s"SUMMARY: $n ERRORS")
    }
    errors.keys.toSeq.sorted foreach { key =>
      println(key)
      errors(key).foreach { err =>
        println(s"  - $err")
      }
      println("")
    }
    println("")
    System.exit(errors.size)
  }

}
