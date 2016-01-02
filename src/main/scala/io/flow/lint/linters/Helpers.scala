package io.flow.lint.linters

import com.bryzek.apidoc.spec.v0.models.{Operation, Resource, Service}

trait Helpers {

  def nonHealthcheckResources(service: Service): Seq[Resource] = {
    service.resources.filter( _.plural != Healthcheck.Plural)
  }

  def error(resource: Resource, error: String): String = {
    s"${resource.plural}: $error"
  }

  def error(resource: Resource, operation: Operation, error: String): String = {
    s"${resource.plural} ${operation.method} ${operation.path}: $error"
  }

}
