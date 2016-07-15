package io.flow.proxy

import io.flow.registry.v0.{Client => RegistryClient}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

/**
  * Cache to lookup information from the registry, ensuring that we
  * contact the registry at most once per service.
  */
private[proxy] case class ServicePortCache(client: RegistryClient)(implicit ec: ExecutionContext) {

  private[this] var cache = scala.collection.mutable.Map[String, Option[Long]]()

  /**
    * Get the port for the specified service, caching the
    * result. Throw an error if there is no port.
    */
  def get(service: String): Long = {
    cache.get(service).getOrElse {
      fetch(service)
    }.getOrElse {
      sys.error(s"Application named[$service] does not have any ports in Registry at[${client.baseUrl}]")
    }
  }

  private[this] def fetch(service: String): Option[Long] = {
    val app = Await.result(
      getFromRegistry(service),
      Duration(3, "seconds")
    ).getOrElse {
      sys.error(s"Could not find application named[$service] in Registry at[${client.baseUrl}]. Either add the service to the registry or, if it should never be part of api.flow.io, add to the proxy whitelist in api-build:src/main/scala/io/flow/proxy/Controller.scala")
    }

    val port = app.ports.headOption.map(_.external)
    cache += (service -> port)
    port
  }
  
  private[this] def getFromRegistry(
    name: String
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ): Future[Option[io.flow.registry.v0.models.Application]] = {
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
