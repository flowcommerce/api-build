package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service

case class Controller() extends io.flow.build.Controller {

  private[this] val linter = Lint()
  private[this] var errors = scala.collection.mutable.Map[String, Seq[String]]()

  override val name = "Linter"

  def run(
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    services.foreach { service =>
      linter.validate(service) match {
        case Nil => println("\n  Valid!")
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
