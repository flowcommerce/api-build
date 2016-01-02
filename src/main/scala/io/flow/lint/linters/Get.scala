package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, Resource, Service}

/**
  * Enforces that each resource has a top level GET method where:
  * 
  * a. All parameters are optional
  * b. First param named "id" with type "[string]"
  * c. last three parameters named limit, offset, sort
  * d. limit is long w/ default of 25, minimum of 1, maximum of 100
  * d. offset is long w/ default of 0, minimum of 0, no maximum
  * e. sort is a string with a default specified
  */
case object Get extends Linter with Helpers {

  private[this] val RequiredParameters = Seq("id", "limit", "offset", "sort")

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).map(validateResource(_)).flatten
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.filter(_.method == Method.Get).sortBy { _.path }.headOption match {
      case None => Seq(error(resource, "Must have at least one operation"))
      case Some(operation) => validateOperation(resource, operation)
    }

    // TODO: Collect all operations that return arrays and validate
    // their parameter lists
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    val requiredErrors = operation.parameters.filter(p => p.required && p.default.isEmpty) match {
      case Nil => Nil
      case params => params.map { p =>
        error(resource, operation, s"Parameter[${p.name}] must be optional or must have a default")
      }
    }

    val paramNames = operation.parameters.map(_.name).filter(name => RequiredParameters.contains(name))
    val missingRequiredParams = RequiredParameters.filter(n => !paramNames.contains(n)).map { name =>
      error(resource, operation, s"Missing parameter[$name]")
    }

    val paramErrors = Seq(
      operation.parameters.find(_.name == "id").map( p =>
        validateParameter(resource, operation, p, "[string]", maximum = Some(25))
      ),
      operation.parameters.find(_.name == "limit").map( p =>
        validateParameter(resource, operation, p, "long", default = Some("25"), minimum = Some(1), maximum = Some(100))
      ),
      operation.parameters.find(_.name == "offset").map( p =>
        validateParameter(resource, operation, p, "long", default = Some("0"), minimum = Some(0), maximum = None)
      )
    ).flatten.flatten

    val positionErrors = missingRequiredParams match {
      case Nil => validateParameterPositions(resource, operation)
      case errors => Nil
    }

    // TODO: Validate responses

    requiredErrors ++ missingRequiredParams ++ paramErrors ++ positionErrors
  }

  // validate id if present is in first position, and the parameter
  // list ends with limit, offset, sort
  private[this] def validateParameterPositions(
    resource: Resource,
    operation: Operation
  ): Seq[String] = {
    val names = operation.parameters.map(_.name)
    val lastThree = names.reverse.take(3).reverse

    Seq(
      names.head == "id" match {
        case true => Nil
        case false => Seq(error(resource, operation, s"Parameter[id] must be the first parameter"))
      },
      lastThree match {
        case "limit" :: "offset" :: "sort" :: Nil => Nil
        case _ => {
          Seq(error(resource, operation, s"last three parameters must be limit, offset, sort and not " + lastThree.mkString(", ")))
        }
      }
    ).flatten
  }

  def validateParameter(
    resource: Resource,
    operation: Operation,
    param: Parameter,
    datatype: String,
    default: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  ): Seq[String] = {
    val typeErrors = param.`type` == datatype match {
      case true => Nil
      case false => {
        Seq(error(resource, operation, s"parameter[${param.name}] must be of type ${datatype} and not ${param.`type`}"))
      }
    }

    val defaultErrors = compare(resource, operation, param, "default", param.default, default)
    val minimumErrors = compare(resource, operation, param, "minimum", param.minimum, minimum)
    val maximumErrors = compare(resource, operation, param, "maximum", param.maximum, maximum)

    typeErrors ++ defaultErrors ++ minimumErrors ++ maximumErrors
  }

  private[this] def compare(
    resource: Resource,
    operation: Operation,
    param: Parameter,
    desc: String,
    actual: Option[Any],
    expected: Option[Any]
  ): Seq[String] = {
    expected match {
      case None => {
        actual match {
          case None => Nil
          case Some(a) => Seq(error(resource, operation, s"parameter[${param.name}] $desc must not be specified. Current value is $a"))
        }
      }
      case Some(value) => {
        actual match {
          case None => Seq(error(resource, operation, s"parameter[${param.name}] $desc is missing. Add with value $value"))
          case Some(a) => {
            a == value match {
              case true => Nil
              case false => Seq(error(resource, operation, s"parameter[${param.name}] $desc must be $value and not $a"))
            }
          }
        }
      }
    }
  }

}
