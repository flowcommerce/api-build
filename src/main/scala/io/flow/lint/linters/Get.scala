package io.flow.lint.linters

import io.flow.lint.Linter
import io.flow.lint.util.Expansions
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, ParameterLocation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

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
  private[this] val ExpandName = "expand"

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).map(validateResource(service, _)).flatten
  }

  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.
      filter(_.method == Method.Get).
      filter(returnsArray(_)).
      flatMap { validateOperation(service, resource, _) }
  }

  def queryParameters(operation: Operation): Seq[Parameter] = {
    operation.parameters.filter(_.location == ParameterLocation.Query)
  }

  def validateOperation(service: Service, resource: Resource, operation: Operation): Seq[String] = {
    val expansions = model(service, operation).map { m =>
      Expansions.fromFieldTypes(m.fields.map(_.`type`))
    }.getOrElse(Nil)

    val allRequiredParameters = expansions match {
      case Nil => RequiredParameters
      case _ => RequiredParameters ++ Seq(ExpandName)
    }

    val requiredErrors = queryParameters(operation).filter(p => p.required && p.default.isEmpty) match {
      case Nil => Nil
      case params => params.map { p =>
        RequiredParameters.contains(p.name) match {
          case true => error(resource, operation, s"Parameter[${p.name}] must be optional")
          case false => error(resource, operation, s"Parameter[${p.name}] must be optional or must have a default")
        }
      }
    }

    val invalidExpandsParameter = expansions match {
      case Nil => {
        queryParameters(operation).map(_.name).contains(ExpandName) match {
          case false => {
            Nil
          }
          case true => {
            Seq(error(resource, operation, s"There are no expansions available - should not have a parameter named $ExpandName"))
          }
        }
      }
      case _ => {
        Nil
      }
    }

    val requiredParamNames = queryParameters(operation).map(_.name).filter(name => allRequiredParameters.contains(name))
    val missingRequiredParams = allRequiredParameters.filter(n => !requiredParamNames.contains(n)) match {
      case Nil => Nil
      case missing => {
        val expandDetail = if (missing.contains(ExpandName)) {
          expansions match {
            case Nil => None
            case names => {
              val ex = names.sorted.mkString(", ")
              val template = s"""{ "name": "$ExpandName", "type": "[string]", "minimum": 0, "maximum": ${names.size}, "example": "$ex", "required": false }"""
              Some(s". Expand template: $template")
            }
          }
        } else {
          None
        }

        val noun = missing.size match {
          case 1 => "parameter"
          case _ => "parameters"
        }

        Seq(error(resource, operation, s"Missing $noun: " + missing.mkString(", ") + expandDetail.getOrElse("")))
      }
    }

    val paramErrors = Seq(
      queryParameters(operation).find(_.name == "id").map( p =>
        validateParameter(service, resource, operation, p, "[string]", maximum = Some(100))
      ),
      queryParameters(operation).find(_.name == "limit").map( p =>
        validateParameter(service, resource, operation, p, "long", default = Some("25"), minimum = Some(1), maximum = Some(100))
      ),
      queryParameters(operation).find(_.name == "offset").map( p =>
        validateParameter(service, resource, operation, p, "long", default = Some("0"), minimum = Some(0), maximum = None)
      ),
      queryParameters(operation).find(_.name == "sort").map( p =>
        validateParameter(service, resource, operation, p, "string", hasDefault = Some(true))
      ),
      invalidExpandsParameter match {
        case Nil => {
          queryParameters(operation).find(_.name == "expand").map( p =>
            expansions match {
              case Nil => validateParameter(service, resource, operation, p, "[string]")
              case names => {
                validateParameter(
                  service,
                  resource,
                  operation,
                  p,
                  "[string]",
                  example = Some(names.sorted.mkString(", ")),
                  minimum = Some(0),
                  maximum = Some(names.size)
                )
              }
            }
          )
        }
        case _ => {
          // Already have errors about the expands parameter... don't
          // validate any further
          None
        }
      }
    ).flatten.flatten

    val positionErrors = Seq(invalidExpandsParameter, missingRequiredParams).flatten match {
      case Nil => {
        val trailingParams = expansions match {
          case Nil => Seq("limit", "offset", "sort")
          case _ => Seq("limit", "offset", "sort", "expand")
        }
        validateParameterPositions(service, resource, operation, trailingParams)
      }
      case errors => Nil
    }

    invalidExpandsParameter ++ missingRequiredParams ++ requiredErrors ++ paramErrors ++ positionErrors
  }

  /**
    * validate id if present is in first position, and the parameter
    *  list ends with the specified expectedTail (e.g. limit, offset, sort)
    */
  private[this] def validateParameterPositions(
    service: Service,
    resource: Resource,
    operation: Operation,
    expectedTail: Seq[String]
  ): Seq[String] = {
    val names = queryParameters(operation).map(_.name)
    val tail = names.reverse.take(expectedTail.size).reverse

    Seq(
      names.head == "id" match {
        case true => Nil
        case false => Seq(error(resource, operation, s"Parameter[id] must be the first parameter"))
      },
      tail == expectedTail match {
        case true => {
          Nil
        }
        case false => {
          Seq(error(resource, operation, s"Last ${expectedTail.size} parameters must be ${expectedTail.mkString(", ")} and not ${tail.mkString(", ")}"))
        }
      }
    ).flatten
  }
  
  def validateParameter(
    service: Service,
    resource: Resource,
    operation: Operation,
    param: Parameter,
    datatype: String,
    hasDefault: Option[Boolean] = None,
    default: Option[String] = None,
    example: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  ): Seq[String] = {
    val typeErrors = param.`type` == datatype match {
      case true => Nil
      case false => {
        Seq(error(resource, operation, s"parameter[${param.name}] must be of type ${datatype} and not ${param.`type`}"))
      }
    }

    val defaultErrors = hasDefault match {
      case None => {
        compare(resource, operation, param, "default", param.default, default)
      }
      case Some(value) => {
        value match {
          case true => {
            param.default match {
              case None => Seq(error(resource, operation, s"parameter[${param.name}] must have a default"))
              case Some(_) => Nil
            }
          }
          case false => {
            param.default match {
              case None => Nil
              case Some(v) => Seq(error(resource, operation, s"parameter[${param.name}] must not have a default. Current value is $v"))
            }
          }
        }
      }
    }

    val exampleErrors = example match {
      case None => Nil
      case Some(ex) => {
        param.example match {
          case None => Seq(error(resource, operation, s"parameter[${param.name}] is missing example. It should be $ex"))
          case Some(actual) => {
            actual == ex match {
              case true => Nil
              case false => Seq(error(resource, operation, s"parameter[${param.name}] example must be[$ex] and not[$actual]"))
            }
          }
        }
      }
    }

    val minimumErrors = compare(resource, operation, param, "minimum", param.minimum, minimum)
    val maximumErrors = compare(resource, operation, param, "maximum", param.maximum, maximum)

    typeErrors ++ defaultErrors ++ exampleErrors ++ minimumErrors ++ maximumErrors
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
          case None => Seq(error(resource, operation, s"parameter[${param.name}] $desc is missing. Expected $value"))
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
