package io.flow.build

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import com.ning.http.client.{AsyncHttpClient, AsyncHttpClientConfig}
import io.apibuilder.api.v0.models.{BatchDownloadApplicationForm, BatchDownloadApplicationsForm}
import io.apibuilder.spec.v0.models.Service

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.{Failure, Success, Try}

/**
  * Utility to download service.json files from API Builder
  */
case class Downloader(config: ApibuilderProfile) {

  def downloadServices(
    applications: Seq[Application]
  )(
    implicit ec: scala.concurrent.ExecutionContext
  ): Either[Seq[String], Seq[Service]] = {
    downloadServices(
      orgKeys = applications.map(_.organization).distinct,
      applications = applications
    )
  }

  @tailrec
  private[this] final def downloadServices(
    orgKeys: Seq[String],
    applications: Seq[Application],
    errors: Seq[String] = Nil,
    services: Seq[Service] = Nil
  )(
    implicit ec: scala.concurrent.ExecutionContext
  ): Either[Seq[String], Seq[Service]] = {
    orgKeys.toList match {
      case Nil => if (errors.isEmpty) {
        Right(services)
      } else {
        Left(errors)
      }
      case one :: rest => {
        downloadBatch(one, applications.filter(_.organization == one)) match {
          case Invalid(newErrors) => downloadServices(rest, applications, errors ++ newErrors.toList, services)
          case Valid(newServices) => downloadServices(rest, applications, errors, services ++ newServices)
        }
      }
    }
  }

  private[this] def withClient[T](f: io.apibuilder.api.v0.Client => T): T = {
    val client = new io.apibuilder.api.v0.Client(
      baseUrl = config.baseUrl,
      auth = config.token.map { token =>
        io.apibuilder.api.v0.Authorization.Basic(token)
      },
      asyncHttpClient = new AsyncHttpClient(
        new AsyncHttpClientConfig.Builder()
          .setExecutorService(java.util.concurrent.Executors.newCachedThreadPool())
          .build()
      )
    )
    try {
      f(client)
    } finally {
      client.closeAsyncHttpClient()
    }
  }


  private[this] def downloadBatch(orgKey: String, applications: Seq[Application])(
    implicit ec: scala.concurrent.ExecutionContext
  ): ValidatedNec[String, Seq[Service]] = {
    assert(
      applications.forall(_.organization == orgKey),
      "All applications must belong to the same org for batch download"
    )

    println(
      s"Downloading API Builder Service Spec for $orgKey: " +
      applications.map(_.applicationVersionLabel).sorted.mkString(" ")
    )
    Try {
      withClient { client =>
        Await.result(
          client.batchDownloadApplications.post(
            orgKey = orgKey,
            batchDownloadApplicationsForm = BatchDownloadApplicationsForm(
              applications = applications.map { a =>
                BatchDownloadApplicationForm(
                  applicationKey = a.application,
                  version = a.version
                )
              }
            )
          ),
          FiniteDuration(45, SECONDS)
        )
      }
    } match {
      case Success(result) => result.applications.map(_.service).validNec
      case Failure(ex) => {
        ex match {
          case io.apibuilder.api.v0.errors.UnitResponse(401) => {
            s"HTTP 401: you are not authorized to download the services for org $orgKey".invalidNec
          }
          case io.apibuilder.api.v0.errors.UnitResponse(404) => {
            s"HTTP 404: services  for org $orgKey not found (or you might not be authorized)".invalidNec
          }
          case _ => {
            ex.printStackTrace()
            s"Error downloading service for org $orgKey: ${ex.getMessage}".invalidNec
          }
        }
      }
    }
  }


  def downloadService(app: Application)(
    implicit ec: scala.concurrent.ExecutionContext
  ): ValidatedNec[String, Service] = {
    downloadBatch(app.organization, Seq(app)).map(_.head)
  }

}
