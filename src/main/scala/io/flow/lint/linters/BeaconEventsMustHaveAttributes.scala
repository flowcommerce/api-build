package io.flow.lint.linters

import io.apibuilder.spec.v0.models._
import io.flow.lint.Linter

/**
  *  All subtypes of the beacon event must have an attributes field
  */
case object BeaconEventsMustHaveAttributes extends Linter with Helpers {

  private[this] val AttributesType = "BeaconAttributes"

  override def validate(service: Service): Seq[String] = {
    service.unions.find(_.name == "event") match {
      case None => Nil
      case Some(u) => {
        val typeNames = u.types.map(_.`type`).toSet
        service.models.filter { m => typeNames.contains(m.name) }.flatMap(validateModel)
      }
    }
  }

  def validateModel(model: Model): Seq[String] = {
    model.fields.find(_.name == "attributes") match {
      case None => {
        Seq(error(model, s"Must have a field named 'attributes' of type '$AttributesType'"))
      }
      case Some(f) => {
        validateField(model, f)
      }
    }
  }

  def validateField(model: Model, field: Field): Seq[String] = {
    val requiredErrors = if (field.required) {
      Seq("not be required")
    } else {
      Nil
    }

    val typeErrors = if (field.`type` == AttributesType) {
      Nil
    } else {
      Seq(s"have type '$AttributesType")
    }

    (requiredErrors ++ typeErrors).toList match {
      case Nil => Nil
      case errors => {
        Seq(error(model, field, "Must " + errors.mkString(" and ")))
      }
    }
  }

}
