package io.flow.lint.util

import io.apibuilder.spec.v0.models.{Field, Model}
import io.flow.lint.linters.Helpers

/**
 * For error models (those models ending with _error), validate:
 *
 *   a. contains a field named errors that is an array
 */
case object ErrorModelsV2 extends Helpers {
  val Version = "2"

  def validateModel(model: Model): Seq[String] = {
    model.fields.find(_.name == "errors") match {
      case None => Seq(error(model, "must contain a field named 'errors'"))
      case Some(f) => validateType(model, f)
    }
  }

  private[this] def validateType(model: Model, field: Field): Seq[String] = {
    if (isArray(field.`type`)) {
      Nil
    } else {
      Seq(error(model, field, s"type must be an array and not '${field.`type`}'"))
    }
  }

}
