package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Service}

/** For well known field names, enforce specific types to ensure consistency. For example, all fields named 'id' must be
  * strings.
  */
case object CommonFieldTypes extends Linter with Helpers {

  private[this] val Expected: Map[String, String] = Map(
    "id" -> "string", // we use string identifiers for all of our resources
    "number" -> "string", // 'number' is the external unique identifier
    "guid" -> "uuid",
    "email" -> "string"
  )

  override def validate(service: Service): Seq[String] = {
    service.models.filter(m => !ignored(m.attributes, "common_field_types")).flatMap(validateModel)
  }

  def validateModel(model: Model): Seq[String] = {
    model.fields.flatMap(validateFieldType(model, _))
  }

  def validateFieldType(model: Model, field: Field): Seq[String] = {
    Expected.get(field.name) match {
      case None => {
        Nil
      }
      case Some(exp) => {
        if (exp == field.`type`) {
          Nil
        } else {
          Seq(error(model, field, s"Type must be '$exp' and not ${field.`type`}"))
        }
      }
    }
  }

}
