package io.flow.lint.linters

import com.bryzek.apidoc.spec.v0.models.{Attribute, Field, Model, Operation, Parameter, Resource, Response, Service, Union}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import play.api.libs.json.{JsError, JsSuccess}

trait Helpers {

  val ExpandName = "expand"

  private val CountryFields = Seq("country", "origin")
  private val CurrencyFields = Seq("currency")
  private val LanguageFields = Seq("language")

  def isCountry(name: String): Boolean = {
    CountryFields.find(name.contains(_)) match {
      case None => false
      case Some(_) => true
    }
  }

  def isCurrency(name: String): Boolean = {
    CurrencyFields.find(name.contains(_)) match {
      case None => false
      case Some(_) => true
    }
  }

  def isLanguage(name: String): Boolean = {
    LanguageFields.find(name.contains(_)) match {
      case None => false
      case Some(_) => true
    }
  }

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
      baseType(response.`type`)
    }
  }

  /**
    * For collections, parses out the base type of the collection.
    */
  def baseType(typ: String): String = {
    val i = typ.lastIndexOf("[")
    if (i < 0) {
      typ
    } else {
      typ.substring(i + 1, typ.indexOf("]"))
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
      isArray(r.`type`) && isSuccess(r)
    } match {
      case None => false
      case Some(_) => true
    }
  }

  /**
    * Returns true if this type is an array, false otherwise
    */
  def isArray(typ: String): Boolean = {
    typ.startsWith("[")
  }

  private[this] val PrimitiveTypes = Set("boolean", "decimal", "integer", "double", "long", "object", "string", "date-iso8601", "date-time-iso8601", "uuid", "unit")
  def isPrimitiveType(typ: String): Boolean = {
    PrimitiveTypes.contains(baseType(typ))
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

  def ignored(attributes: Seq[Attribute], name: String): Boolean = {
    attributeIgnore(attributes).contains(name)
  }

  private[this] def attributeIgnore(attributes: Seq[Attribute]): Seq[String] = {
    attributes.find(_.name == "linter") match {
      case None => {
        Nil
      }

      case Some(attr) => {
        (attr.value \ "ignore").validate[Seq[String]] match {
          case e: JsError => Nil
          case s: JsSuccess[Seq[String]] => s.get
        }
      }
    }
  }
  
}
