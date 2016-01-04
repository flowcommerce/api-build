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
          case e: java.io.FileNotFoundException => {
            Left("URL not found or you were not authorized to download that file")
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




