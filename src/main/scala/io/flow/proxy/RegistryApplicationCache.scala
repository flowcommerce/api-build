package io.flow.proxy

import io.flow.registry.v0.{Client => RegistryClient}
import io.flow.registry.v0.models.Application
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

/**
  * Cache to lookup information from the registry, ensuring that we
  * contact the registry at most once per service.
  */
private[proxy] case class RegistryApplicationCache(client: RegistryClient)(implicit ec: ExecutionContext) {

  private[this] var cache = scala.collection.mutable.Map[String, Option[Application]]()

  /**
    * Get the port for the specified service, caching the
    * result. Throw an error if there is no port.
    *
    * @param name Application name in registry
    */
  def get(name: String): Option[Application] = {
    println(cache)
    cache.getOrElse(
      name, {
        //attempt to load/reload cache and 'get' again, otherwise, fail
        loadCache(100, 0)
        cache.getOrElse(name, sys.error(s"After loading the cache, application named[$name] was still not available"))
      }
    )
  }


  def externalPort(name: String): Long = {
    val app = get(name).getOrElse {
      sys.error(s"Could not find application named[$name] in Registry at[${client.baseUrl}]. Either add the application to the registry or, if it should never be part of api.flow.io, add to the proxy whitelist in api-build:src/main/scala/io/flow/proxy/Controller.scala")
    }
    app.ports.headOption.map(_.external).getOrElse {
      sys.error(s"Application named[$name] does not have any ports in Registry at[${client.baseUrl}]")
    }
  }

  def loadCache(limit: Long, offset: Long): Unit = {
    def load(limit: Long, offset: Long): Unit = {
      Await.result(
        getFromRegistry(limit, offset),
        Duration(5, "seconds")
      ).foreach{ apps =>
        if (apps.nonEmpty)
          load(limit, offset + limit)

        apps.map(app =>
          cache += (app.id -> Some(app))
        )
      }
    }

    load(limit, offset)
  }

  def loadCacheWithFold(limit: Long, offset: Long): Unit = {
    Await.result(
      getFromRegistry(limit, offset),
      Duration(5, "seconds")
    ).foldLeft(cache){ (acc, apps) =>
      apps.isEmpty match {
        case false => loadCacheWithFold(limit, offset + limit)
        case true => //no-op
      }
      apps.map(app =>
        cache += (app.id -> Some(app))
      )
      cache
    }
  }
  
  private[this] def getFromRegistry(limit: Long, offset: Long) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Future[Option[Seq[Application]]] = {
    client.applications.get(limit = limit, offset = offset).map { apps =>
      Some(apps)
    }.recover {
      case io.flow.registry.v0.errors.UnitResponse(404) => {
        None
      }

      case ex: Throwable => {
        sys.error(s"Error fetching applications from registry at[${client.baseUrl}]; $ex")
      }
    }
  }
}
