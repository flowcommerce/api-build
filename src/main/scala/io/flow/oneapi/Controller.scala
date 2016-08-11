package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.{BuildType, Downloader}

case class Controller() extends io.flow.build.Controller {

  override val name = "OneApi"
  override val command = "oneapi"

  def run(
    buildType: BuildType,
    downloader: Downloader,
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    println("Building single API from: " + services.map(_.name).mkString(", "))
    OneApi(buildType, services).process match {
      case Left(errs) => {
        errs.foreach { addError(_) }
      }

      case Right(service) => {
        import com.bryzek.apidoc.spec.v0.models.json._
        import play.api.libs.json._

        val path = s"/tmp/flow-${buildType}.json"
        new java.io.PrintWriter(path) { write(Json.prettyPrint(Json.toJson(service))); close }
        println(s"One API file created. See: $path")
      }
    }
  }

}
