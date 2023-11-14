package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Field, Model, Service}
import io.flow.lint.Linter

/** Mapping models create qualified associations between two models. We enforce naming such that
  * <model1>_<model2>_mapping must have the fields:
  *
  *   a. id b. model1 of type model1_reference c. model2 of type model2_reference
  */
case object MappingModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.filter { m => isMapping(m.name) }.flatMap(validateModel)
  }

  private[this] def validateModel(model: Model): Seq[String] = {
    model.fields.toList match {
      case f1 :: f2 :: f3 :: _ => {
        val typeErrors = validateTypes(f1, f2, f3)

        val nameErrors = typeErrors.toList match {
          case Nil => validateNames(f1, f2, f3)
          case _ => Nil
        }

        val modelNameErrors = (typeErrors ++ nameErrors).toList match {
          case Nil => validateModelName(s"${f2.name}_${f3.name}_mapping", model.name)
          case _ => Nil
        }

        typeErrors ++ nameErrors ++ modelNameErrors
      }
      case _ => {
        Seq(
          error(model, "Mapping models must have at least 3 fields"),
        )
      }
    }
  }

  private[this] def validateTypes(f1: Field, f2: Field, f3: Field): Seq[String] = {
    val f1Errors = if (f1.`type` == "string") {
      Nil
    } else {
      Seq(s"Field '${f1.name}' type must be 'string'")
    }

    val f2Errors = if (isReference(f2.`type`)) {
      Nil
    } else {
      Seq(s"Field '${f2.name}' type must be '${f2.name}_reference'")
    }

    val f3Errors = if (isReference(f3.`type`)) {
      Nil
    } else {
      Seq(s"Field '${f3.name}' type must be '${f3.name}_reference'")
    }

    f1Errors ++ f2Errors ++ f3Errors
  }

  private[this] def isReference(typ: String): Boolean = {
    typ.endsWith("_reference")
  }

  private[this] def validateNames(f1: Field, f2: Field, f3: Field): Seq[String] = {
    validateName(1, "id", f1.name) ++
      validateName(2, stripReference(stripPackage(f2.`type`)), f2.name) ++
      validateName(3, stripReference(stripPackage(f3.`type`)), f3.name)
  }

  private[this] def validateName(index: Int, expected: String, actual: String): Seq[String] = {
    if (expected == actual) {
      Nil
    } else {
      Seq(s"Field $index '$actual' must be named '$expected'")
    }
  }

  private[this] def stripReference(typ: String): String = {
    typ.stripSuffix("_reference")
  }

  private[this] def stripPackage(typ: String): String = {
    typ.split("\\.").last
  }

  private[this] def validateModelName(expected: String, actual: String): Seq[String] = {
    if (expected == actual) {
      Nil
    } else {
      Seq(s"Model '$actual' must be named '$expected'")
    }
  }

}
