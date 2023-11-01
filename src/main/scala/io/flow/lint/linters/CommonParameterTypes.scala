package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Method, Operation, Parameter, Resource, Service}

/** For well known parameters, enforce specific types, defaults, minimums and maximums. Validates only GET operations.
  */
case object CommonParameterTypes extends Linter with Helpers {

  case class Spec(
    typ: String,
    default: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  )

  private[this] val Expected: Map[String, Spec] = Map(
    "id" -> Spec("[string]", default = None, minimum = None, maximum = Some(100)),
    "limit" -> Spec("long", default = Some("25"), minimum = Some(1), maximum = Some(100)),
    "offset" -> Spec("long", default = Some("0"), minimum = Some(0), maximum = None)
  )

  private[this] val Types: Map[String, String] = Map(
    "sort" -> "string",
    "expand" -> "[string]"
  )

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).flatMap(validateResource)
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations
      .filter(_.method == Method.Get)
      .filter(returnsArray)
      .filter(op => !ignored(op.attributes, "common_parameter_types"))
      .flatMap { op =>
        op.parameters.flatMap { param =>
          validateParameter(resource, op, param)
        }
      }
  }

  def validateParameter(resource: Resource, op: Operation, param: Parameter): Seq[String] = {
    Expected.get(param.name) match {
      case None => {
        Types.get(param.name) match {
          case None => Nil
          case Some(typ) => {
            if (typ == param.`type`) {
              Nil
            } else {
              Seq(error(resource, op, param, s"Type expected[$typ] but found[${param.`type`}]"))
            }
          }
        }
      }

      case Some(spec) => {
        compare(resource, op, param, "Type", Some(param.`type`), Some(spec.typ)) ++
          compare(resource, op, param, "Default", param.default.map(_.toString), spec.default.map(_.toString)) ++
          compare(resource, op, param, "Minimum", param.minimum, spec.minimum) ++
          compare(resource, op, param, "Maximum", param.maximum, spec.maximum)
      }
    }
  }

  private[this] def compare[T](
    resource: Resource,
    op: Operation,
    param: Parameter,
    label: String,
    actual: Option[T],
    expected: Option[T]
  ): Seq[String] = {
    (actual, expected) match {
      case (None, None) => Nil
      case (None, Some(e)) => Seq(error(resource, op, param, s"$label was not specified - should be $e"))
      case (Some(_), None) => Seq(error(resource, op, param, s"$label should not be specified"))
      case (Some(a), Some(e)) => {
        if (a == e) {
          Nil
        } else {
          if (param.name == "id" && op.path.endsWith("/versions") && param.`type` == "[long]") {
            // Special case as versions use journal id which is a long
            Nil
          } else {
            Seq(error(resource, op, param, s"$label expected[$e] but found[$a]"))
          }
        }
      }
    }
  }

}
