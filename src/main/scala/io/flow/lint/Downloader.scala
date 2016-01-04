package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import scala.util.{Failure, Success, Try}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Utility to download service.json files from apidoc
  */
case class Downloader() {

  private[this] val apidocApiToken = Environment.optionalString("APIDOC_API_TOKEN")
  private[this] val apidocApiHost = Environment.optionalString("APIDOC_API_HOST").getOrElse("http://api.apidoc.me")

  apidocApiToken match {
    case None => println("NO API TOKEN")
    case Some(t) => println(s"FOUND TOKEN: $t")
  }

  private[this] val client = {
    new com.bryzek.apidoc.api.v0.Client(
      apiUrl = apidocApiHost,
      auth = apidocApiToken.map { token =>
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
        client.versions.getByOrgKeyAndApplicationKeyAndVersion(organization, application, version).map { v =>
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

  def withClient(f: Downloader => Unit) {
    val downloader = Downloader()
    try {
      f(downloader)
    } finally {
      downloader.close()
    }
  }

}




