package io.flow.build

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.lint
import io.flow.oneapi

object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  private[this] val controllers = Seq(
    lint.Controller(),
    oneapi.Controller()
  )

  ApidocConfig.load() match {
    case Left(error) => {
      println(s"** Error loading apidoc config: $error")
      System.exit(1)
    }

    case Right(profile) => {
      args.toList match {
        case Nil => {
          println("** ERROR: Specify lint | oneapi | all")
        }

        case one :: rest => {
          val selected = if (one == "all") { controllers } else { controllers.filter(_.name == one) }
          selected.toList match {
            case Nil => {
              println(s"** ERROR: Invalid command[$one]. Must be one of: lint | build | all")
            }

            case _ => {
              val services = Downloader.withClient(profile) { dl =>
                rest.flatMap { name =>
                  Application.parse(name) match {
                    case None => {
                      println(s"** WARNING: Could not find application[$name]")
                      None
                    }

                    case Some(app) => {
                      dl.service(app) match {
                        case Left(error) => {
                          println(s"** WARNING: Failed to download app[${app.label}]: $error")
                          None
                        }
                        case Right(service) => {
                          Some(service)
                        }
                      }
                    }
                  }
                }
              }

              run(selected, services)
            }
          }
        }
      }
    }
  }

  private[this] def run(controllers: Seq[Controller], services: Seq[Service]) {


    var numberErrors = 0

    controllers.foreach { controller =>
      controller.run(services)
      val errors = controller.errors()
      numberErrors += errors.size

      println("==================================================")
      println(s"controller: ${controller.name}")
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
    }
    System.exit(numberErrors)
  }

}
