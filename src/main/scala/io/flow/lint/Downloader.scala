package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import scala.util.{Failure, Success, Try}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Utility to download service.json files from apidoc
  */
case class Downloader(config: ApidocProfile) {

  private[this] val client = {
    new com.bryzek.apidoc.api.v0.Client(
      baseUrl = config.baseUrl,
      auth = config.token.map { token =>
        com.bryzek.apidoc.api.v0.Authorization.Basic(token)
      }
    )
  }

  def close() {
    client.closeAsyncHttpClient()
  }

  def service(
    organization: String,
    application: String,
    version: String = "latest"
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Either[String, Service] = {
    Try(
      Await.result(
        client.versions.getByApplicationKeyAndVersion(organization, application, version).map { v =>
          v.service
        },
        Duration(5, "seconds")
      )
    ) match {
      case Success(value) => Right(value)
      case Failure(ex) => {
        ex match {
          case com.bryzek.apidoc.api.v0.errors.UnitResponse(401) => {
            Left("HTTP 401: you are not authorized to download this service")
          }
          case com.bryzek.apidoc.api.v0.errors.UnitResponse(404) => {
            Left("HTTP 404: service not found (or you might not be authorized)")
          }
          case _ => {
            Left(s"Error: $ex")
          }
        }
      }
    }
  }

}

object Downloader {

  def withClient(
    config: ApidocProfile
  ) (
    f: Downloader => Unit
  ) {
    val downloader = Downloader(config)
    try {
      f(downloader)
    } finally {
      downloader.close()
    }
  }

}




