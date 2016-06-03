package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models.Service

case class Controller() extends io.flow.build.Controller {

  private[this] val Org = "flow"

  private[this] val Specs = Seq(
    "common", "experience", "location", "reference", "tracking",
    "catalog", "delivery_window", "fulfillment", "organization", "search", "user"
  )

  override val name = "OneApi"

  def run(
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    OneApi(services).process match {
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
