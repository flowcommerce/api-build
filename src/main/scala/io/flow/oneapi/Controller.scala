package io.flow.oneapi

import io.apibuilder.spec.v0.models.Service
import io.flow.build.{Application, BuildType, Downloader, Config}

case class Controller() extends io.flow.build.Controller {

  override val name = "OneApi"
  override val command = "oneapi"

  def run(
    config: Config,
    downloader: Downloader,
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    val eventService: Seq[Service] = (
      config.buildType match {
        case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiPartner | BuildType.ApiMiscEvent => None
        case BuildType.Api => Some(BuildType.ApiEvent.toString)
        case BuildType.ApiInternal => Some(BuildType.ApiInternalEvent.toString)
        case BuildType.ApiMisc => Some(BuildType.ApiMiscEvent.toString)
      }
    ) match {
      case None => Nil
      case Some(name) => downloader.service(Application("flow", name, "latest")) match {
        case Left(errors) => sys.error(s"Failed to download apibuilder application flow/$name: $errors")
        case Right(service) => Seq(service)
      }
    }

    val all = services ++ eventService
    println("Building single API from: " + all.map(_.name).mkString(", "))
    OneApi(config.buildType, all).process match {
      case Left(errs) => {
        println(s"Errors from building single API:\n - ${errs.mkString("\n")}")
        errs.foreach { addError(_) }
      }

      case Right(service) => {
        import io.apibuilder.spec.v0.models.json._
        import play.api.libs.json._

        val path = s"/tmp/flow-${config.buildType}.json"
        new java.io.PrintWriter(path) {
          write(Json.prettyPrint(Json.toJson(service)))
          close
        }
        println(s"One API file created. See: $path")
      }
    }
  }

}
