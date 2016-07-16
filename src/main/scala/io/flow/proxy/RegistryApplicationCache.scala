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
    * @param name Application name in registy
    */
  def get(name: String): Option[Application] = {
    cache.get(name).getOrElse {
      fetch(name)
    }
  }

  def externalPort(name: String): Long = {
    val app = get(name).getOrElse {
      sys.error(s"Could not find application named[$name] in Registry at[${client.baseUrl}]. Either add the application to the registry or, if it should never be part of api.flow.io, add to the proxy whitelist in api-build:src/main/scala/io/flow/proxy/Controller.scala")
    }
    app.ports.headOption.map(_.external).getOrElse {
      sys.error(s"Application named[$name] does not have any ports in Registry at[${client.baseUrl}]")
    }
  }

  private[this] def fetch(name: String): Option[Application] = {
    val app = Await.result(
      getFromRegistry(name),
      Duration(3, "seconds")
    )
    cache += (name -> app)
    app
  }
  
  private[this] def getFromRegistry(
    name: String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Future[Option[Application]] = {
    client.applications.getById(name).map { app =>
      Some(app)
    }.recover {
      case io.flow.registry.v0.errors.UnitResponse(404) => {
        None
      }

      case ex: Throwable => {
        sys.error(s"Error fetching application[$name] from registry at[${client.baseUrl}]; $ex")
      }
    }
  }
}
