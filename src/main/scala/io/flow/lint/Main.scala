package io.flow.lint

object Main extends App {

  private[this] val linter = Lint()

  Downloader.withClient { dl =>

    import scala.concurrent.ExecutionContext.Implicits.global

    args.foreach { name =>
      val (organization, application, version) = name.split("/").map(_.trim).toList match {
        case org :: app :: Nil => (org, app, "latest")
        case org :: app :: version :: Nil => (org, app, version)
        case _ => {
          sys.error(s"Invalid name[$name] - expected organization/application (e.g. flow/user)")
        }
      }

      println("")
      println(s"$name")
      print(s"  Downloading...")
      dl.service(organization, application, version) match {
        case Left(error) => {
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
                println(s"    - $error")
              }
            }
          }
        }
      }
    }

  }
}
