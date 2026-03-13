package io.flow.oneapi

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all._
import io.apibuilder.spec.v0.models.Service
import io.flow.build.{Application, BuildConfig, BuildType, DownloadCache}

case class Controller() extends io.flow.build.Controller {

  override val name = "OneApi"
  override val command = "oneapi"

  def run(
    buildType: BuildType,
    buildConfig: BuildConfig,
    downloadCache: DownloadCache,
    services: Seq[Service],
  )(implicit
    ec: scala.concurrent.ExecutionContext,
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

    validateVersionConsistency(services) match {
      case Invalid(errs) =>
        errs.toNonEmptyList.toList.foreach(addError)
        return
      case Valid(_) => ()
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

        val path = buildConfig.output.resolve(s"flow-$buildType.json").toFile
        new java.io.PrintWriter(path) {
          write(Json.prettyPrint(Json.toJson(service)))
          close()
        }
        println(s"One API file created. See: $path")
      }
    }
  }

  private def validateVersionConsistency(services: Seq[Service]): cats.data.ValidatedNec[String, Unit] = {
    val versions = services.map(_.version).distinct
    versions.toList match {
      case Nil | _ :: Nil => ().validNec
      case _ =>
        val versionCounts = services.groupBy(_.version).view.mapValues(_.size).toMap
        val expectedVersion = versionCounts.maxBy(_._2)._1
        val outliers = services.filterNot(_.version == expectedVersion)
        val msg = s"Version inconsistency detected. Expected all services at version $expectedVersion but found: " +
          outliers.map(s => s"${s.name}@${s.version}").mkString(", ")
        msg.invalidNec
    }
  }

}
