package io.flow.lint.linters

import com.bryzek.apidoc.spec.v0.models.{Model, Operation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

trait Helpers {

  def nonHealthcheckResources(service: Service): Seq[Resource] = {
    service.resources.filter( _.plural != Healthcheck.Plural)
  }

  def error(model: Model, error: String): String = {
    s"Model ${model.name}: $error"
  }

  def error(resource: Resource, error: String): String = {
    s"Resource ${resource.plural}: $error"
  }

  def error(resource: Resource, operation: Operation, error: String): String = {
    s"Resource ${resource.plural} ${operation.method} ${operation.path}: $error"
  }

  def error(resource: Resource, operation: Operation, response: Response, error: String): String = {
    val label = response.code match {
      case ResponseCodeInt(n) => s"Response $n"
      case ResponseCodeOption.Default => "Response default"
      case ResponseCodeOption.UNDEFINED(name) => s"Response $name"
      case ResponseCodeUndefinedType(name) => s"Response $name"
    }
    s"Resource ${resource.plural} ${operation.method} ${operation.path} $label: $error"
  }

}
