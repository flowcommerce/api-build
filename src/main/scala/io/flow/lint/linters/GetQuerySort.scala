package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Operation, Parameter, ParameterLocation, Resource, Service}
import io.flow.lint.Linter

/**
  * Enforces that all GET methods that have a 'q' param do NOT have
  * a 'sort' param. We do this because q indicates search; with algolia
  * there is no way to do a dynamic sort, and pre-indexing all the sort
  * resulted in too large a working set - and thus we cannot offer
  * sorting.
  */
case object GetQuerySort extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).map(validateResource(service, _)).flatten
  }

  private[this] def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.
      filter( op => hasQueryParameter(op, "q")).
      filter( op => hasQueryParameter(op, "sort")).
      map { op =>
        error(resource, op, s"Parameter[sort] is not supported for operations that contain a 'q' parameter")
      }
  }

  private[this] def hasQueryParameter(operation: Operation, name: String): Boolean = {
    operation.parameters.exists { p =>
      p.location == ParameterLocation.Query && p.name == name
    }
  }
}
