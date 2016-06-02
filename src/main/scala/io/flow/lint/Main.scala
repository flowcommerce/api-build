package io.flow.lint

import io.flow.build.Downloader

case class Config(organization: String, application: String, version: String)

case class Controller(dl: Downloader) extends io.flow.build.Controller {

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

  override val name = "Linter"

  def run(
    args: Seq[String]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    args.foreach { name =>
      name.split("/").map(_.trim).toList match {
        case org :: app :: Nil => {
          process(org, app, "latest")
        }

        case _ => {
          val msg = s"Invalid name[$name] - expected organization/application (e.g. flow/user)"
          addError("arguments", msg)
          println(s"** ERROR: $msg")
        }
      }
    }
  }

  def process(
    org: String, app: String, version: String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    println("")
    println(s"$org/$app")
    print(s"  Downloading...")
    dl.service(org, app, version) match {
      case Left(error) => {
        addError(org, app, error)
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
              addError(org, app, error)
              println(s"    - $error")
            }
          }
        }
      }
    }
  }
}
