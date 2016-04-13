package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, Resource, Service}

/**
  * For well known parameters, enforce specific types, defaults,
  * minimums and maximums. Validates only GET operations.
  */
case object CommonParameterTypes extends Linter with Helpers {

  case class Spec(
    typ: String,
    default: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  )

  val Expected = Map(
    "id" -> Spec("[string]", default = None, minimum = None, maximum = Some(100)),
    "limit" -> Spec("long", default = Some("25"), minimum = Some(1), maximum = Some(100)),
    "offset" -> Spec("long", default = Some("0"), minimum = Some(0), maximum = None)
  )

  val Types = Map(
    "sort" -> "string",
    "expand" -> "[string]"
  )

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).map(validateResource(service, _)).flatten
  }

  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.
      filter(_.method == Method.Get).
      filter(returnsArray(_)).
      flatMap { op =>
        op.parameters.flatMap { param =>
          validateParameter(service, resource, op, param)
        }
      }
  }

  def validateParameter(service: Service, resource: Resource, op: Operation, param: Parameter): Seq[String] = {
    Expected.get(param.name) match {
      case None => {
        Types.get(param.name) match {
          case None => Nil
          case Some(typ) => {
            typ == param.`type` match {
              case true => Nil
              case false => Seq(error(resource, op, param, s"Type expected[$typ] but found[${param.`type`}]"))
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

  private[this] def compare[T](resource: Resource, op: Operation, param: Parameter, label: String, actual: Option[T], expected: Option[T]): Seq[String] = {
    (actual, expected) match {
      case (None, None) => Nil
      case (None, Some(e)) => Seq(error(resource, op, param, s"$label was not specified - should be $e"))
      case (Some(_), None) => Seq(error(resource, op, param, s"$label should not be specified"))
      case (Some(a), Some(e)) => {
        a == e match {
          case true => Nil
          case false => Seq(error(resource, op, param, s"$label expected[$e] but found[$a]"))
        }
      }
    }
  }

}
