package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models.Service
import io.flow.build.{Application, BuildType, Downloader}

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
    val eventService: Seq[Service] = buildType match {
      case BuildType.Api | BuildType.ApiInternal => Seq(
        downloader.service(Application("flow", buildType.toString, "latest")) match {
          case Left(errors) => sys.error(s"Failed to download events: $errors")
          case Right(service) => service
        }
      )
      case BuildType.ApiEvent | BuildType.ApiInternalEvent => Nil
    }
    
    println("Building single API from: " + services.map(_.name).mkString(", "))
    OneApi(buildType, services ++ eventService).process match {
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
