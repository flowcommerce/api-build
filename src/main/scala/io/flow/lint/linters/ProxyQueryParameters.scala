package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Operation, Parameter, Resource, Service}

/**
  * We reserve 'method' and 'callback' for jsonp requests (as
  * implemented by github.com/flowvault/proxy)
  * 
  * 'envelope' reserved to wrap responses in an envelope (as HTTP 200)
  */
case object ProxyQueryParameters extends Linter with Helpers {

  private[this] val ReservedNames: Seq[String] = Seq("callback", "envelope", "method")

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).flatMap(validateResource)
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.flatMap { op =>
      op.parameters.flatMap { param =>
        validateParameter(resource, op, param)
      }
    }
  }

  def validateParameter(resource: Resource, op: Operation, param: Parameter): Seq[String] = {
    if (ReservedNames.contains(param.name)) {
      Seq(error(resource, op, param, s"name is reserved for use only in https://github.com/flowvault/proxy"))
    } else {
      Nil
    }
  }

}
