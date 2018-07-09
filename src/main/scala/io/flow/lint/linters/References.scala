package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Model, Service, Union, Enum}
import io.flow.lint.Linter

case object References extends Linter with Helpers {
  /**
    * Validates that for every "xyz_reference", there is a type named "xyz"
    */
  override def validate(service: Service): Seq[String] = {
    val modelErrors = service.models
      .filter(m => !ignored(m.attributes, "reference_type_exists"))
      .flatMap(validateModel(service, _))

    val unionErrors = service.unions
      .filter(m => !ignored(m.attributes, "reference_type_exists"))
      .flatMap(validateUnion(service, _))

    val enumErrors = service.enums
      .filter(m => !ignored(m.attributes, "reference_type_exists"))
      .flatMap(validateEnum(service, _))

    modelErrors ++ unionErrors ++ enumErrors
  }

  private def validateModel(service: Service, model: Model) =
    validateName(service, model.name).map(error(model, _))

  private def validateUnion(service: Service, union: Union) =
    validateName(service, union.name).map(error(union, _))

  private def validateEnum(service: Service, enum: Enum) =
    validateName(service, enum.name).map(error(enum, _))

  private def validateName(service: Service, name: String): Seq[String] = {
    if (name.endsWith("_reference")) {
      val fullName = name.substring(0, name.length - "_reference".length)

      if (service.models.exists(_.name == fullName)
        || service.enums.exists(_.name == fullName)
        || service.unions.exists(_.name == fullName))
        Seq()
      else
        Seq(s"No such type $fullName. References need to be named after existing types")
    } else {
      Seq()
    }
  }
}
