package io.flow.lint.linters

import com.bryzek.apidoc.spec.v0.models.{Field, Model, Operation, Parameter, Resource, Response, Service, Union}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

trait Helpers {

  val ExpandName = "expand"

  val CountryField = "country"
  val CurrencyField = "currency"

  /**
    * Returns the model for this resource. Right now only will resolve
    * if the model is defined directly in the service (i.e. not
    * imported)
    */
  def model(service: Service, resource: Resource): Option[Model] = {
    service.models.find(_.plural == resource.plural)
  }

  /**
    * Returns the model for the successful response type for this
    * operation. Right now only will resolve if the model is defined
    * directly in the service (i.e. not imported)
    */
  def model(service: Service, operation: Operation): Option[Model] = {
    responseType(service, operation).flatMap { t =>
      service.models.find(_.name == t)
    }
  }

  /**
    * Returns the name of the datatype for the successful response for this
    * operation.
    */
  def responseType(service: Service, operation: Operation): Option[String] = {
    operation.responses.find(isSuccess(_)).map { response =>
      val i = response.`type`.lastIndexOf("[")
      if (i < 0) {
        response.`type`
      } else {
        response.`type`.substring(i + 1, response.`type`.indexOf("]"))
      }
    }
  }

  def nonHealthcheckResources(service: Service): Seq[Resource] = {
    service.resources.filter( _.plural != "healthchecks")
  }

  def error(union: Union, error: String): String = {
    s"Union ${union.name}: $error"
  }

  def error(model: Model, error: String): String = {
    s"Model ${model.name}: $error"
  }

  def error(model: Model, field: Field, error: String): String = {
    s"Model ${model.name} Field[${field.name}]: $error"
  }

  def error(resource: Resource, error: String): String = {
    s"Resource ${resource.plural}: $error"
  }

  def error(resource: Resource, operation: Operation, error: String): String = {
    s"Resource ${resource.plural} ${operation.method} ${operation.path}: $error"
  }

  def error(resource: Resource, operation: Operation, parameter: Parameter, error: String): String = {
    s"Resource ${resource.plural} ${operation.method} ${operation.path} Parameter ${parameter.name}: $error"
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

  /**
    * Returns true if this operation has a 2xx response that returns
    * an array of items.
    */
  def returnsArray(operation: Operation): Boolean = {
    operation.responses.find { r =>
      r.`type`.startsWith("[") && isSuccess(r)
    } match {
      case None => false
      case Some(_) => true
    }
  }

  /**
    * Returns true if this response represents a 2xx
    */
  def isSuccess(response: Response): Boolean = {
    response.code match {
      case ResponseCodeInt(n) => n >= 200 && n < 300
      case ResponseCodeOption.Default => true
      case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
    }
  }

}
