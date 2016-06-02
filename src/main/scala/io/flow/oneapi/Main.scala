package io.flow.oneapi

import io.flow.lint.{ApidocConfig, Config, Downloader}

object Main extends App {

  val Specs = Seq(
    "flow/common", "flow/experience", "flow/location", "flow/reference", "flow/tracking",
    "flow/catalog", "flow/delivery_window", "flow/fulfillment", "flow/organization", "flow/search", "flow/user"
  )

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
                    import com.bryzek.apidoc.spec.v0.models.json._
                    import play.api.libs.json._

                    println("Done")

                    val path = "/tmp/flow-api.json"
                    new java.io.PrintWriter(path) { write(Json.prettyPrint(Json.toJson(service))); close }
                    println(s"See: $path")
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
