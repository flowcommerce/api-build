package io.flow.lint

object Main extends App {

  private[this] val linter = Lint()
  private[this] var errors = scala.collection.mutable.Map[String, Seq[String]]()

  private[this] def addError(organization: String, application: String, error: String) {
    addError(s"$organization/$application", error)
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

  private[this] case class Config(organization: String, application: String, version: String)

  ApidocConfig.load() match {
    case Left(error) => println(s"** Error loading apidoc config: $error")
    case Right(config) => {
      Downloader.withClient(config) { dl =>

        import scala.concurrent.ExecutionContext.Implicits.global

        args.foreach { name =>
          val configOption = name.split("/").map(_.trim).toList match {
            case org :: app :: Nil => Some(Config(org, app, "latest"))
            case org :: app :: version :: Nil => Some(Config(org, app, version))
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
                addError(config.organization, config.application, error)
                println("\n  ** ERROR: " + error)
              }
              case Right(service) => {
                print("  Done\n  Starting Linter... ")
                linter.validate(service) match {
                case Nil => println("\n  Valid!")
                  case errors => {
                    errors.size match {
                      case 1 => println(" 1 error:")
                      case n => println(s" $n errors:")
                    }
                    errors.sorted.foreach { error =>
                      addError(config.organization, config.application, error)
                      println(s"    - $error")
                    }
                  }
                }
              }
            }
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
