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
        val defaultErrors = field.default.map(_.toString) == spec.default.map(_.toString) match {
          case true => Nil
          case false => Seq(error(model, field, s"Default must be '${spec.default}' and not ${field.default}"))
        }

        val minimumErrors = field.minimum == spec.minimum match {
          case true => Nil
          case false => Seq(error(model, field, s"Minimum must be '${spec.minimum}' and not ${field.minimum}"))
        }

        val maximumErrors = field.maximum == spec.maximum match {
          case true => Nil
          case false => Seq(error(model, field, s"Maximum must be '${spec.maximum}' and not ${field.maximum}"))
        }
        
        defaultErrors ++ minimumErrors ++ maximumErrors
      }
    }
  }

}
