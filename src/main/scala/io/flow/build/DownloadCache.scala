package io.flow.build

import io.apibuilder.spec.v0.models.Service

import scala.collection.concurrent.TrieMap

case class DownloadCache(downloader: Downloader)(
  implicit ec: scala.concurrent.ExecutionContext
) {

  private[this] val cache = TrieMap[Application, Service]()

  def downloadServices(
    applications: Seq[Application]
  ): Either[Seq[String], Seq[Service]] = {
    val (cached, remaining) = applications.partition(cache.isDefinedAt)
    val services = cached.map { a =>
      cache.getOrElse(a, sys.error("Invalid cache entry for application"))
    }
    downloader.downloadServices(remaining).map { rest =>
      rest.foreach { s =>
        println("Adding service to cache: "+ Application.latest(s.organization.key, s.application.key))
        cache.put(Application.latest(s.organization.key, s.application.key), s)
      }
      services ++ rest
    }
  }

  def downloadService(app: Application): Either[Seq[String], Service] = {
    downloadServices(Seq(app)).map(_.head)
  }

}
