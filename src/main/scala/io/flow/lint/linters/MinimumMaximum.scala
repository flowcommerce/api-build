package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Operation, Parameter, Resource, Service}
import scala.util.{Failure, Success, Try}

/**   - validates that any field with a maximum has the maximum set to 100
  *   - validates that any field with a minimum has the minimum set to 0 or 1
  *   - if there is a numeric default and a minimum, validates that default >= minimum
  *   - every parameter that is an array should have a maximum of 100 (except if it is the expand parameter)
  */
case object MinimumMaximum extends Linter with Helpers {

  val GlobalMax = 100

  val CountryMax = 3
  val CurrencyMax = 3
  val LanguageMax = 2

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel) ++ service.resources.flatMap(validateResource(service, _))
  }

  def validateModel(model: Model): Seq[String] = {
    model.fields.flatMap { validateField(model, _) }
  }

  def validateField(model: Model, field: Field): Seq[String] = {
    val minErrors = field.minimum match {
      case None => Nil
      case Some(min) => {
        field.default match {
          case None => Nil
          case Some(default) => {
            Try(
              default.toLong,
            ) match {
              case Success(d) => {
                if (d < min) {
                  Seq(error(model, field, s"Default must be >= minimum[$min] and not $default"))
                } else {
                  Nil
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

    val maxErrors = field.maximum match {
      case None => Nil
      case Some(max) =>
        field.name match {
          case c if isCountry(c) =>
            if (max == CountryMax) {
              Nil
            } else {
              Seq(error(model, field, s"Maximum must be $CountryMax and not $max"))
            }

          case c if isCurrency(c) =>
            if (max == CurrencyMax) {
              Nil
            } else {
              Seq(error(model, field, s"Maximum must be $CurrencyMax and not $max"))
            }

          case c if isLanguage(c) =>
            if (max == LanguageMax) {
              Nil
            } else {
              Seq(error(model, field, s"Maximum must be $LanguageMax and not $max"))
            }

          case _ =>
            field.minimum match {
              case Some(min) => {
                if (max >= min) {
                  Nil
                } else {
                  Seq(error(model, field, s"Maximum, if specified with minimum, must be >= $min and not $max"))
                }
              }
              case None => Nil
            }
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
        if (min < 0) {
          Seq(error(resource, operation, param, s"Minimum must be >= 0 and not $min"))
        } else {
          Nil
        }
      }
    }

    val maxErrors = param.maximum match {
      case None => {
        if (isArray(param.`type`)) {
          Seq(error(resource, operation, param, s"Missing maximum"))
        } else {
          Nil
        }
      }

      case Some(max) => {
        if (isArray(param.`type`)) {
          val desiredMax = service.enums.find(_.name == baseType(param.`type`)) match {
            case Some(enum) => Some(enum.values.size)
            case None => {
              if (isPrimitiveType(param.`type`)) {
                Some(GlobalMax)
              } else {
                None
              }
            }
          }

          desiredMax match {
            case None => Nil
            case Some(expected) => {
              if (max == expected) {
                Nil
              } else if (param.name == ExpandName) {
                Nil
              } else {
                Seq(error(resource, operation, param, s"Maximum must be $expected and not $max"))
              }
            }
          }
        } else {
          Nil
        }
      }
    }

    minErrors ++ maxErrors
  }

}
