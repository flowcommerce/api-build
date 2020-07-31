package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import io.flow.build.{BuildType, Downloader}

case class Controller() extends io.flow.build.Controller {

  override val name = "Linter"
  override val command = "lint"

  def run(
    buildType: BuildType,
    downloader: Downloader,
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Unit = {
    services.foreach { service =>
      Lint(buildType).validate(service) match {
        case Nil => println(" Valid!")
        case errors => {
          println(s"Lint.service ${service.namespace}: ${errors}")
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
