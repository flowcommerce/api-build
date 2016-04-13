package io.flow.lint.linters

import io.flow.lint.Linter
import io.flow.lint.util.Expansions
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, ParameterLocation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

/**
  * Enforces that each resource has a top level GET method where one
  * of the following is true
  * 
  *  a. first param named 'q' with type 'string'
  *  b. first param named "id" with type "[string]" and
  *     last three parameters named limit, offset, sort
  */
case object Get extends Linter with Helpers {

  trait Sublinter {
    def validateOperation(service: Service, resource: Resource, operation: Operation): Seq[String]
  }

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).map(validateResource(GetPrimary, service, _)).flatten match {
      case Nil => {
        Nil // todo
      }

      case errors => errors
    }
  }

  def validateResource(sublinter: Sublinter, service: Service, resource: Resource): Seq[String] = {
    resource.operations.
      filter(_.method == Method.Get).
      filter(returnsArray(_)).
      flatMap { sublinter.validateOperation(service, resource, _) }
  }
  
  def queryParameters(operation: Operation): Seq[Parameter] = {
    operation.parameters.filter(_.location == ParameterLocation.Query)
  }


  /**
    *  b. first param named "id" with type "[string]" and
    *     last three parameters named limit, offset, sort
    */
  case object GetPrimary extends Sublinter with Helpers {

    private[this] val RequiredParameters = Seq("id", "limit", "offset", "sort")

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

      val expandErrors: Seq[String] = expansions match {
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
          queryParameters(operation).find(_.name == "expand").map( p =>
            expansions match {
              case Nil => {
                Nil
              }

              case names => {
                val requiredExample = names.sorted.mkString(", ")
                val exampleErrors = p.example == requiredExample match {
                  case true => Nil
                  case false => {
                    p.example match {
                      case None =>Seq(error(resource, operation, s"parameter[expand] is missing example. It must be $requiredExample"))
                      case value => Seq(error(resource, operation, s"parameter[expand] must have example[$requiredExample] and not[$value]"))
                    }
                  }
                }

                val maximumErrors = p.maximum == Some(names.size) match {
                  case true => Nil
                  case false => {
                    p.maximum match {
                      case None => Seq(error(resource, operation, s"parameter[expand] is missing maximum. It must be ${names.size}"))
                      case value => Seq(error(resource, operation, s"parameter[expand] must have maximum[${names.size}] and not[$value]"))
                    }
                  }
                }

                exampleErrors ++ maximumErrors
              }
            }
          ).getOrElse(Nil)
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

      val positionErrors = Seq(expandErrors, missingRequiredParams).flatten match {
        case Nil => {
          val trailingParams = expansions match {
            case Nil => Seq("limit", "offset", "sort")
            case _ => Seq("limit", "offset", "sort", "expand")
          }
          validateParameterPositions(service, resource, operation, trailingParams)
        }
        case errors => Nil
      }

      expandErrors ++ missingRequiredParams ++ requiredErrors ++ positionErrors
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

      names.head == "q" match {
        case true => Nil
        case false => {
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
      }
    }
    
  }
}
