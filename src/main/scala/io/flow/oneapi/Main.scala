package io.flow.oneapi

import io.flow.lint.{ApidocConfig, Config, Downloader}

object Main extends App {

  val Specs = Seq("flow/common", "flow/organization", "flow/catalog", "flow/user")

  private[this] var errors = scala.collection.mutable.Map[String, Seq[String]]()
  private[this] val GlobalError = "Global"

  private[this] def addError(organization: String, application: String, error: String) {
    addError(s"$organization/$application", error)
  }

  private[this] def addError(message: String) {
    addError(GlobalError, message)
  }
  
  private[this] def addError(key: String, error: String) {
    errors.get(key) match {
      case None => {
        errors.put(key, Seq(error))
      }
      case Some(existing) => {
        errors.put(key, existing ++ Seq(error))
      }
    }
  }
  
  ApidocConfig.load() match {
    case Left(error) => {
      println(s"** Error loading apidoc config: $error")
    }

    case Right(config) => {
      Downloader.withClient(config) { dl =>

        import scala.concurrent.ExecutionContext.Implicits.global

        val services = Specs.flatMap { name =>
          val configOption = name.split("/").map(_.trim).toList match {
            case org :: app :: Nil => Some(Config(org, app, "latest"))
            case _ => {
              val msg = s"Invalid name[$name] - expected organization/application (e.g. flow/user)"
              addError("arguments", msg)
              println(s"** ERROR: $msg")
              None
            }
          }

          configOption.map { config =>
            println("")
            println(s"$name")
            print(s"  Downloading...")

            dl.service(config.organization, config.application, config.version) match {
              case Left(error) => {
                print(s" error\n")
                addError(config.organization, config.application, error)
                None
              }

              case Right(service) => {
                print(s" done\n")
                Some(service)
              }
            }
          }
        }

        errors.toList match {
          case Nil => {
            services.flatten.toList match {
              case Nil => {
                addError("At least one service must be specified")
              }
              case svcs => {
                println("")
                OneApi(svcs).process match {
                  case Left(errs) => {
                    errs.foreach { addError(_) }
                  }
                  case Right(service) => {
                    println("Done")
                  }
                }
              }
            }
          }
          case _ => {
            // handled below
          }
        }
      }
    }
  }

  println("")
  println("==================================================")
  errors.size match {
    case 0 => println(s"SUMMARY: NO ERRORS")
    case 1 => println(s"SUMMARY: 1 ERROR")
    case n => println(s"SUMMARY: $n ERRORS")
  }
  println("==================================================")
  errors.keys.toSeq.sorted foreach { app =>
    println(app)
    errors(app).foreach { err =>
      println(s"  - $err")
    }
    println("")
  }


  System.exit(errors.size)
}
