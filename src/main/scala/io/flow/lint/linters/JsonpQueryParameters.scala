package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, Resource, Service}

/**
  * We reserve 'method' and 'callback' for jsonp requests (as
  * implemented by github.com/flowvault/proxy)
  */
case object JsonpQueryParameters extends Linter with Helpers {

  val ReservedNames = Seq("method", "callback")

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).map(validateResource(service, _)).flatten
  }

  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.flatMap { op =>
      op.parameters.flatMap { param =>
        validateParameter(service, resource, op, param)
      }
    }
  }

  def validateParameter(service: Service, resource: Resource, op: Operation, param: Parameter): Seq[String] = {
    ReservedNames.contains(param.name) match {
      case false => Nil
      case true => Seq(error(resource, op, param, s"name is reserved for use only in jsonp"))
    }
  }

}
