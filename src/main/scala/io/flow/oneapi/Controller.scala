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
    val eventService: Seq[Service] = (
      buildType match {
        case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiPartner => None
        case BuildType.Api => Some(BuildType.ApiEvent.toString)
        case BuildType.ApiInternal => Some(BuildType.ApiInternalEvent.toString)
      }
    ) match {
      case None => Nil
      case Some(name) => downloader.service(Application("flow", name, "latest")) match {
        case Left(errors) => sys.error(s"Failed to download apidoc application flow/$name: $errors")
        case Right(service) => Seq(service)
      }
    }

    val all = services ++ eventService
    println("Building single API from: " + all.map(_.name).mkString(", "))
    OneApi(buildType, all).process match {
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
