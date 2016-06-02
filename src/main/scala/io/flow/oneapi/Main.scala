package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.Downloader

case class Controller(dl: Downloader) extends io.flow.build.Controller {

  private[this] val Org = "flow"

  private[this] val Specs = Seq(
    "common", "experience", "location", "reference", "tracking",
    "catalog", "delivery_window", "fulfillment", "organization", "search", "user"
  )

  override val name = "OneApi"

  def run(
    args: Seq[String]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    val services: Seq[Service] = Specs.flatMap { name =>
      print(s"Downloading $Org/$name")
      dl.service(Org, name, "latest") match {
        case Left(error) => {
          print(s" error\n")
          addError(Org, name, error)
          None
        }

        case Right(service) => {
          print(s" done\n")
          Some(service)
        }
      }
    }

    services.toList match {
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

}
