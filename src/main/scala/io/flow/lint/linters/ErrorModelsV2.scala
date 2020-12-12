package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Field, Model, Service}
import io.flow.lint.Linter

/**
 * Models ending with _errors, validate:
 *   a. contains a field named errors that is an array
 *   b. the type of the array is a model ending in _error
 *
 * Models ending with _error, validate:
 *   a. contains a field named code whose type is an enum
 *   b. contains a field name message whose type is a string
 */
case object ErrorModelsV2 extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models
      .filter { m => !ignored(m.attributes, "error") }
      .flatMap { m =>
      if (m.name.endsWith("_errors")) {
        validateWrapper(m)
      } else if (m.name.endsWith("_error") && !errorVersion(m.attributes).contains(1)) {
        validateModel(m)
      } else {
        Nil
      }
    }
  }

  private[this] def validateWrapper(model: Model): Seq[String] = {
    model.fields.find(_.name == "errors") match {
      case None => Seq(error(model, "must contain a field named 'errors'"))
      case Some(f) => validateWrapperType(model, f)
    }
  }

  private[this] def validateWrapperType(model: Model, field: Field): Seq[String] = {
    if (isArray(field.`type`)) {
      val fieldType = baseType(field.`type`)
      if (fieldType.endsWith("_error")) {
        Nil
      } else {
        // TODO: Verify type is a model
        Seq(error(model, field, s"type '${field.`type`}' must end in '_error'"))
      }
    } else {
      Seq(error(model, field, s"type must be an array and not '${field.`type`}'"))
    }
  }

  private[this] def validateModel(model: Model): Seq[String] = {
    validateModelCode(model) ++ validateModelMessage(model)
  }

  private[this] def validateModelCode(model: Model): Seq[String] = {
    model.fields.find(_.name == "code") match {
      case None => Seq(error(model, "must contain a field named 'code'"))
      case Some(_) => {
        // TODO: resolve field type and ensure an enum
        Nil
      }
    }
  }

  private[this] def validateModelMessage(model: Model): Seq[String] = {
    model.fields.find(_.name == "message") match {
      case None => Seq(error(model, "must contain a field named 'message'"))
      case Some(f) => {
        if (f.`type` == "string") {
          Nil
        } else {
          Seq(error(model, f, s"type must be 'string' and not '${f.`type`}'"))
        }
      }
    }
  }

}
