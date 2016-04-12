package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Field, Model, Service}

/**
  * For well known field names, enforce specific parameters for
  * consistency. For example, all fields named 'limit' must have a
  * minimum of 1, default of 25 and maximum of 25.
  */
case object CommonFieldParameters extends Linter with Helpers {

  case class Spec(
    default: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  )

  val Expected = Map(
    "limit" -> Spec(default = Some("25"), minimum = Some(1), maximum = Some(100)),
    "offset" -> Spec(default = Some("0"), minimum = Some(0), maximum = None)
  )

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel(service, _))
  }

  def validateModel(service: Service, model: Model): Seq[String] = {
    model.fields.flatMap(validateFieldType(service, model, _))
  }

  def validateFieldType(service: Service, model: Model, field: Field): Seq[String] = {
    Expected.get(field.name) match {
      case None => {
        Nil
      }

      case Some(spec) => {
        compare(model, field, "Default", field.default.map(_.toString), spec.default.map(_.toString)) ++
        compare(model, field, "Minimum", field.minimum, spec.minimum) ++
        compare(model, field, "Maximum", field.maximum, spec.maximum)
      }
    }
  }

  private[this] def compare[T](model: Model, field: Field, label: String, actual: Option[T], expected: Option[T]): Seq[String] = {
    (actual, expected) match {
      case (None, None) => Nil
      case (None, Some(_)) => Seq(error(model, field, s"$label should not be specified"))
      case (Some(_), None) => Seq(error(model, field, s"$label mising"))
      case (Some(a), Some(e)) => {
        a == e match {
          case true => Nil
          case false => Seq(error(model, field, s"$label expected[$e] but found[$a]"))
        }
      }
    }
  }

}
