package io.flow.proxy

import io.flow.registry.v0.models.Application
import io.flow.registry.v0.{Client => RegistryClient}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Cache to lookup information from the registry. This cache is filled on instantiation and will not pick up any
  * applications added to the registry after loading (i.e. there is no refresh).
  */
private[proxy] case class RegistryApplicationCache(
  client: RegistryClient,
)(implicit ec: ExecutionContext) {

  private[this] val cache: Map[String, Application] = load(
    cache = scala.collection.mutable.Map[String, Application](),
    offset = 0,
  )

  /** Get the application with the specified name from the registry.
    *
    * @param name
    *   Application name in registry
    */
  def get(name: String): Option[Application] = {
    cache.get(name)
  }

  /** Returns the external port for the application with the specified name, throwing an error if the application does
    * not exist in the registry.
    *
    * @param registryName
    *   Application name in registry (ex. experience)
    * @param serviceName
    *   Actual service name (ex. experience-internal)
    */
  def externalPort(registryName: String, serviceName: String): Long = {
    val app = get(registryName).getOrElse {
      sys.error(
        s"Could not find application named[$serviceName] in Registry at[${client.baseUrl}]. Either add the application to the registry or add an attribute named api-build/host to the apibuilder spec. The context of this error is that as we are building the proxy configuration file, we do not know how to route traffic to the resources defined in this spec.",
      )
    }
    app.ports.headOption.map(_.external).getOrElse {
      sys.error(s"Application named[$registryName] does not have any ports in Registry at[${client.baseUrl}]")
    }
  }

  @tailrec
  private[this] def load(
    cache: scala.collection.mutable.Map[String, Application],
    offset: Long,
  ): Map[String, Application] = {
    val limit = 100L
    val results = Await.result(
      getFromRegistry(
        limit = limit,
        offset = offset,
      ),
      Duration(5, "seconds"),
    )

    results.map { apps =>
      apps.map { app =>
        cache += (app.id -> app)
      }
    }

    if (results.map(_.size).getOrElse(0) >= limit) {
      load(cache, offset + limit)
    } else {
      cache.toMap
    }
  }

  private[this] def getFromRegistry(limit: Long, offset: Long)(implicit
    ec: scala.concurrent.ExecutionContext,
  ): Future[Option[Seq[Application]]] = {
    client.applications
      .get(limit = limit, offset = offset)
      .map { apps =>
        Some(apps)
      }
      .recover {
        case io.flow.registry.v0.errors.UnitResponse(404) => {
          None
        }

        case ex: Throwable => {
          sys.error(s"Error fetching applications from registry at[${client.baseUrl}]; $ex")
        }
      }
  }
}
