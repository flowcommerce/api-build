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
          val selected = if (one == "all") { controllers } else { controllers.filter(_.command == one) }
          selected.toList match {
            case Nil => {
              println(s"** ERROR: Invalid command[$one]. Must be one of: " + controllers.map(_.command).mkString(", "))
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

    var errors = scala.collection.mutable.Map[String, Seq[String]]()

    controllers.foreach { controller =>
      println("==================================================")
      println(s"${controller.name} Starting")
      println("==================================================")

      controller.run(services)
      controller.errors.foreach {
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
