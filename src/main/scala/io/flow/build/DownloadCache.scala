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
    def cacheKey(a: Application) = Application.latest(a.organization, a.application)

    val (cached, remaining) = applications.partition { a => cache.isDefinedAt(cacheKey(a)) }

    downloader.downloadServices(remaining).map { rest =>
      rest.foreach { s =>
        cache.put(Application.latest(s.organization.key, s.application.key), s)
      }
      cached.map { a =>
        cache.getOrElse(cacheKey(a), sys.error("Invalid cache entry for application"))
      } ++ rest
    }
  }

  def downloadService(app: Application): Either[Seq[String], Service] = {
    downloadServices(Seq(app)).map(_.head)
  }

}
