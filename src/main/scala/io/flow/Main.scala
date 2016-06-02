package io.flow.build

import io.flow.lint
import io.flow.oneapi

object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  ApidocConfig.load() match {
    case Left(error) => {
      println(s"** Error loading apidoc config: $error")
      System.exit(1)
    }

    case Right(profile) => {
      Downloader.withClient(profile) { dl =>
        args.toList match {
          case Nil => {
            println("** ERROR: Specify lint | oneapi | all")
          }

          case one :: rest => {
            one match {
              case "lint" => run(Seq(lint.Controller(dl)), rest)
              case "oneapi" => run(Seq(oneapi.Controller(dl)), rest)
              case "all" => run(Seq(lint.Controller(dl), oneapi.Controller(dl)), rest)
              case other => println(s"** ERROR: Invalid command[$other]. Must be one of: lint | build | all")
            }
          }
        }
      }
    }
  }

  private[this] def run(controllers: Seq[Controller], args: Seq[String]) {
    var numberErrors = 0

    controllers.foreach { controller =>
      controller.run(args)
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
