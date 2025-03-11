package io.flow.build

import io.apibuilder.spec.v0.models.{Import, Service}

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

case class DownloadCache(downloader: SpecProvider)(implicit
  ec: scala.concurrent.ExecutionContext,
) {

  private[this] val cache = TrieMap[Application, Service]()

  private[this] def cacheKey(a: Application) = Application.latest(a.organization, a.application)
  private[this] def isDefinedAt(application: Application): Boolean = cache.isDefinedAt(cacheKey(application))
  private[this] def toApplication(imp: Import): Application =
    Application.latest(imp.organization.key, imp.application.key)

  @tailrec
  final def downloadAllServicesAndImports(services: Seq[Service], index: Int = 0): Seq[Service] = {
    val all = services ++ mustDownloadServices(
      services.flatMap(_.imports).map(toApplication),
    )
    val missing = all.flatMap(_.imports).map(toApplication).filterNot(isDefinedAt)
    if (missing.isEmpty) {
      all
    } else {
      downloadAllServicesAndImports(all, index + 1)
    }
  }

  def mustDownloadServices(applications: Seq[Application]): Seq[Service] = {
    downloadServices(applications) match {
      case Left(errors) => sys.error(s"Failed to download services: ${errors.mkString(", ")}")
      case Right(services) => services
    }
  }

  def downloadServices(
    applications: Seq[Application],
  ): Either[Seq[String], Seq[Service]] = {

    val (cached, remaining) = applications.partition { a => isDefinedAt(cacheKey(a)) }

    downloader.downloadServices(remaining.distinct).map { rest =>
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
