package io.flow.oneapi

import cats.data.Validated.{Invalid, Valid}
import io.apibuilder.spec.v0.models.Service
import io.flow.build.{Application, BuildType, DownloadCache}

case class Controller() extends io.flow.build.Controller {

  override val name = "OneApi"
  override val command = "oneapi"

  def run(
    buildType: BuildType,
    downloadCache: DownloadCache,
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Unit = {
    val eventService: Seq[Service] = (
      buildType match {
        case BuildType.ApiEvent | BuildType.ApiInternalEvent | BuildType.ApiPartner | BuildType.ApiMiscEvent => None
        case BuildType.Api => Some(BuildType.ApiEvent.toString)
        case BuildType.ApiInternal => Some(BuildType.ApiInternalEvent.toString)
        case BuildType.ApiMisc => Some(BuildType.ApiMiscEvent.toString)
      }
    ) match {
      case None => Nil
      case Some(applicationKey) => {
        downloadCache.downloadService(Application.latest("flow", applicationKey)) match {
          case Left(errors) => sys.error(s"Failed to download API Builder application flow/$applicationKey: $errors")
          case Right(service) => Seq(service)
        }
      }
    }

    val all = services ++ eventService
    println("Building single API from: " + all.map(_.name).mkString(", "))
    OneApi(buildType, downloadCache, all).process() match {
      case Invalid(errs) => {
        println(s"Errors from building single API:\n - ${errs.toNonEmptyList.toList.mkString("\n")}")
        errs.toNonEmptyList.toList.foreach(addError)
      }

      case Valid(service) => {
        import io.apibuilder.spec.v0.models.json._
        import play.api.libs.json._

        val path = s"/tmp/flow-$buildType.json"
        new java.io.PrintWriter(path) {
          write(Json.prettyPrint(Json.toJson(service)))
          close()
        }
        println(s"One API file created. See: $path")
      }
    }
  }

}
