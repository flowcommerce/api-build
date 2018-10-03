package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import io.flow.build.{BuildType, Downloader, Config}

case class Controller() extends io.flow.build.Controller {

  private[this] var errors = scala.collection.mutable.Map[String, Seq[String]]()

  override val name = "Linter"
  override val command = "lint"

  def run(
    config: Config,
    downloader: Downloader,
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    services.foreach { service =>
      print(s"${service.name}...")

      Lint(config.buildType).validate(service) match {
        case Nil => println(" Valid!")
        case errors => {
          errors.size match {
            case 1 => println(" 1 error:")
            case n => println(s" $n errors:")
          }
          errors.sorted.foreach { error =>
            addError(service.name, error)
            println(s"    - $error")
          }
        }
      }
    }
  }
}
