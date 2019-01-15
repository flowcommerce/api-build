package io.flow.build

import io.apibuilder.spec.v0.models.Service
import scala.util.{Failure, Success, Try}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Utility to download service.json files from apibuilder
  */
case class Downloader(config: ApibuilderProfile) {

  private[this] val client = {
    new io.apibuilder.api.v0.Client(
      baseUrl = config.baseUrl,
      auth = config.token.map { token =>
        io.apibuilder.api.v0.Authorization.Basic(token)
      }
    )
  }

  def close(): Unit = {
    client.closeAsyncHttpClient()
  }

  def service(
    app: Application
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Either[String, Service] = {
    Try {
      print(s"${app.label} Downloading...")
      val result = Await.result(
        client.versions.getByApplicationKeyAndVersion(app.organization, app.application, app.version).map { v =>
          v.service
        },
        Duration(15, "seconds")
      )
      println(" Done")
      result
    } match {
      case Success(value) => Right(value)
      case Failure(ex) => {
        ex match {
          case io.apibuilder.api.v0.errors.UnitResponse(401) => {
            Left("HTTP 401: you are not authorized to download this service")
          }
          case io.apibuilder.api.v0.errors.UnitResponse(404) => {
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

  def withClient[T](
    profile: ApibuilderProfile
  ) (
    f: Downloader => T
  ) = {
    val downloader = Downloader(profile)
    try {
      f(downloader)
    } finally {
      downloader.close()
    }
  }

}
