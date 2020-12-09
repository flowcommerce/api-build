package io.flow.build

import io.apibuilder.spec.v0.models.Service
import io.flow.{oneapi, lint, proxy, stream}

object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  private[this] var globalErrors = scala.collection.mutable.ListBuffer[String]()

  private[this] def controllers(buildType: BuildType): Seq[Controller] = {
    val all = scala.collection.mutable.ListBuffer[Controller]()
    all.append(lint.Controller())
    all.append(stream.Controller())
    if (buildType.oneapi) {
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
    case Right(profile) => {
      implicit val buildTypeRead: scopt.Read[BuildType] =
        scopt.Read.reads(s => BuildType.fromString(s).getOrElse(sys.error(s"Unknown BuildType '$s'")))

      val parser = new scopt.OptionParser[Config]("api-build") {
        override def showUsageOnError = Some(true)

        arg[BuildType]("<build type>")
          .action((bt, c) => c.copy(buildType = bt))
          .text("One of: " + BuildType.all.map(_.toString).mkString(", "))
          .required()

        arg[String]("<build command>")
          .action((cmd, c) => c.copy(buildCommand = cmd))
          .text("One of: " + controllers(BuildType.Api).map(_.command).mkString("all, ", ", ", ""))
          .required()

        arg[String]("<flow/experience>...")
          .text("API specs from APIBuilder")
          .action((api, c) => c.copy(apis = c.apis :+ api))
          .unbounded()
          .optional()
          .validate(api => Application.parse(api) match {
            case Some(_) => success
            case None => failure(s"Could not parse application[$api]")
          })

        help("help")

        checkConfig(c => {
          val selected = if (c.buildCommand == "all") {
            controllers(c.buildType)
          } else {
            controllers(c.buildType).filter(_.command == c.buildCommand)
          }
          selected.toList match {
            case Nil => {
              failure(s"Invalid command[${c.buildCommand}] for build type[${c.buildType}]. " +
                s"Must be one of: all, " + controllers(c.buildType).map(_.command).mkString(", "))
            }
            case _ => {
              success
            }
          }
        })
      }

      parser.parse(args, Config()) match {
        case Some(config) =>
          val selected = if (config.buildCommand == "all") {
            controllers(config.buildType)
          } else {
            controllers(config.buildType).filter(_.command == config.buildCommand)
          }

          Downloader.withClient(profile) { dl =>
            val services = config.apis.flatMap { name =>
              Application.parse(name).flatMap { app =>
                dl.service(app) match {
                  case Left(error) => {
                    globalErrors += s"Failed to download app[${app.label}]: $error"
                    None
                  }
                  case Right(service) => {
                    Some(service)
                  }
                }
              }
            }
            run(config.buildType, dl, selected, services)
          }
        case None =>
          // error message already printed
      }
    }
  }

  private[this] def run(buildType: BuildType, downloader: Downloader, controllers: Seq[Controller], services: Seq[Service]): Unit = {
    val errors = scala.collection.mutable.Map[String, Seq[String]]()
    if (globalErrors.nonEmpty) {
      errors += ("config" -> globalErrors.toSeq)
    }

    controllers.foreach { controller =>
      println("==================================================")
      println(s"${controller.name} Starting")
      println("==================================================")

      controller.run(buildType, downloader, services)
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
