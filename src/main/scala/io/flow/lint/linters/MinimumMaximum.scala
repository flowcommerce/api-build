package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Field, Model, Operation, Parameter, Resource, Service}
import scala.util.{Failure, Success, Try}

/**
  *  - validates that any field with a maximum has the maximum set to 100
  *  - validates that any field with a minimum has the minimum set to 0 or 1
  *  - if there is a numeric default and a minimum, validates that
  *    default >= minimum
  *  - every parameter that is an array should have a maximum of 100
  *    (except if it is the expand parameter)
  */
case object MinimumMaximum extends Linter with Helpers {

  val GlobalMax = 100

  val CountryMax = 3
  val CurrencyMax = 3
  val LanguageMax = 2

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel(service, _)) ++ service.resources.flatMap(validateResource(service, _))
  }

  def validateModel(service: Service, model: Model): Seq[String] = {
    model.fields.flatMap { validateField(model, _) }
  }

  def validateField(model: Model, field: Field): Seq[String] = {
    val minErrors = field.minimum match {
      case None => Nil
      case Some(min) => {
        if (min < 0) {
          Seq(error(model, field, s"Minimum must be >= 0 and not $min"))
        } else {
          field.default match {
            case None => Nil
            case Some(default) => {
              Try(
                default.toLong
              ) match {
                case Success(default) => {
                  default < min match {
                    case false => Nil
                    case true => Seq(error(model, field, s"Default must be >= minimum[$min] and not $default"))
                  }
                }
                case Failure(_) => {
                  // Not a number - nothing to validate
                  Nil
                }
              }
            }
          }
        }
      }
    }

    val maxErrors = field.maximum match {
      case None => Nil
      case Some(max) =>  field.name match {
        case c if isCountry(c) =>
          max == CountryMax match {
            case true => Nil
            case false => Seq(error(model, field, s"Maximum must be $CountryMax and not $max"))
          }

        case c if isCurrency(c) =>
          max == CurrencyMax match {
            case true => Nil
            case false => Seq(error(model, field, s"Maximum must be $CurrencyMax and not $max"))
          }

        case c if isLanguage(c) =>
          max == LanguageMax match {
            case true => Nil
            case false => Seq(error(model, field, s"Maximum must be $LanguageMax and not $max"))
          }

        case _ =>
          // Nothing else to validate for fields
          Nil
      }
    }

    minErrors ++ maxErrors
  }
  
  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.flatMap(validateOperation(service, resource, _))
  }

  def validateOperation(service: Service, resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.flatMap(validateParameter(service, resource, operation, _))
  }

  def validateParameter(service: Service, resource: Resource, operation: Operation, param: Parameter): Seq[String] = {
    val minErrors = param.minimum match {
      case None => Nil
      case Some(min) => {
        min < 0 match {
          case true => Seq(error(resource, operation, param, s"Minimum must be >= 0 and not $min"))
          case false => Nil
        }
      }
    }

    val maxErrors = param.maximum match {
      case None => Nil
      case Some(max) => {
        max == GlobalMax match {
          case true => Nil
          case false => {
            param.name == ExpandName match {
              case false => {
                Seq(error(resource, operation, param, s"Maximum must be $GlobalMax and not $max"))
              }
              case true => {
                Nil
              }
            }
          }
        }
      }
    }

    val missingMaxErrors = param.maximum match {
      case None => {
        isArray(param.`type`) match {
          case false => {
            Nil
          }
          case true => {
            Seq(error(resource, operation, param, s"Missing maximum. All parameters that are arrays must have a maximum set to $GlobalMax"))
          }
        }
      }
      case Some(_) => {
        Nil
      }
    }

    minErrors ++ maxErrors ++ missingMaxErrors
  }

  def isArray(datatype: String): Boolean = {
    datatype.indexOf("[") >= 0
  }

}
