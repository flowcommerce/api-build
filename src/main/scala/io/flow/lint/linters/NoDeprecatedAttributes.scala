package io.flow.lint.linters

import io.apibuilder.spec.v0.models._
import io.flow.lint.Linter

/**
  *  Ensures that attributes we no longer support are not specified
  */
case object NoDeprecatedAttributes extends Linter {

  private[this] val Deprecated = Set("auth")

  override def validate(service: Service): Seq[String] = {
    val all: Seq[Attribute] = service.headers.flatMap(_.attributes) ++
      service.enums.flatMap(_.attributes) ++
      service.interfaces.flatMap(_.attributes) ++
      service.unions.flatMap(_.attributes) ++
      service.models.flatMap(_.attributes) ++
      service.resources.flatMap(_.attributes) ++
      service.resources.flatMap(_.operations).flatMap(_.attributes) ++
      service.resources.flatMap(_.operations).flatMap(_.parameters).flatMap(_.attributes.getOrElse(Nil)) ++
      service.resources.flatMap(_.operations).flatMap(_.responses).flatMap(_.attributes.getOrElse(Nil))

    println(s"all: ${all}")
    all.flatMap(validateAttribute).distinct
  }

  private[this] def validateAttribute(attr: Attribute): Seq[String] = {
    if (Deprecated.contains(attr.name)) {
      Seq(s"Service contains a deprecated attribute named '${attr.name}' - remove this attribute")
    } else {
      Nil
    }
  }

}
