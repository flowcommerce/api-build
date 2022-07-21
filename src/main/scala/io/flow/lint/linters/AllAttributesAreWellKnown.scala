package io.flow.lint.linters

import io.apibuilder.spec.v0.models._
import io.flow.lint.Linter

/**
  *  Ensures that attributes we no longer support are not specified
  */
case object AllAttributesAreWellKnown extends Linter {

  private[this] val KnownAttributeNames = Set(
    "api-build",
    "linter",
    "sort"
  )

  override def validate(service: Service): Seq[String] = {
    allAttributes(service).flatMap(validateAttribute).distinct
  }

  private[this] def validateAttribute(attr: Attribute): Seq[String] = {
    if (KnownAttributeNames.contains(attr.name)) {
      Nil
    } else {
      Seq(errorMessage(attr.name))
    }
  }

  private[this] def errorMessage(attributeName: String): String = {
    s"Service contains an unknown attribute named '$attributeName' - remove this attribute or add to AllAttributesAreWellKnown.KnownAttributeNames in the api-build project (https://github.com/flowcommerce/api-build)"
  }

  private[this] def allAttributes(service: Service): Seq[Attribute] = {
    service.headers.flatMap(_.attributes) ++
      service.enums.flatMap(_.attributes) ++
      service.interfaces.flatMap(_.attributes) ++
      service.unions.flatMap(_.attributes) ++
      service.models.flatMap(_.attributes) ++
      service.resources.flatMap(_.attributes) ++
      service.resources.flatMap(_.operations).flatMap(_.attributes) ++
      service.resources.flatMap(_.operations).flatMap(_.parameters).flatMap(_.attributes.getOrElse(Nil)) ++
      service.resources.flatMap(_.operations).flatMap(_.responses).flatMap(_.attributes.getOrElse(Nil))
  }
}
