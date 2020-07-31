package io.flow.lint.linters

import io.apibuilder.spec.v0.models._
import io.flow.lint.Linter

/**
 * With diversity in mind and in an effort to foster an inclusive and safe environment,
 * this linter ensures we avoid the usage of words that can be offensive.
 *
 * @see [RFC](https://docs.google.com/document/d/1V33mFQETX_XalLcGjE7m9kP_LzCwHslHlYlCGi3RqSc/edit)
 */
case object InclusiveTerminologyLinter extends Linter with Helpers {

  private[this] val Suggestions = Map(
    "whitelist" -> "allowlist",
    "blacklist" -> "denylist",
    "master" -> "primary or leader",
    "slave" -> "secondary or follower",
    "dummy" -> "placeholder",
    "sanity" -> "completeness",
    "young" -> "junior",
    "old" -> "senior",
  )
  // Note leaving 'gender' out of linter as capturing gender is a valid use case

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel) ++
      service.enums.flatMap(validateEnum) ++
      service.unions.flatMap(validateUnion) ++
      service.interfaces.flatMap(validateInterface) ++
      service.headers.flatMap(validateHeader) ++
      service.resources.flatMap(validateResource)
  }

  def validateInterface(interface: Interface): Seq[String] = {
    validateName(interface.name) { m => error(interface, m) } ++ interface.fields.flatMap { validateField(interface, _) }
  }

  def validateField(interface: Interface, field: Field): Seq[String] = {
    validateName(field.name) { m => error(interface, field, m) }
  }

  def validateHeader(header: Header): Seq[String] = {
    validateName(header.name)  { m => error(header, m) }
  }

  def validateEnum(enum: Enum): Seq[String] = {
    validateName(enum.name) { m => error(enum, m) } ++ enum.values.flatMap { validateEnumValue(enum, _) }
  }

  def validateEnumValue(enum: Enum, enumValue: EnumValue): Seq[String] = {
    validateName(enumValue.name) { m => error(enum, enumValue, m) } ++ (enumValue.value match {
      case None => Nil
      case Some(v) => validateName(v) { m => error(enum, enumValue, s"value: $m") }
    })
  }

  def validateModel(model: Model): Seq[String] = {
    validateName(model.name) { m => error(model, m) } ++ model.fields.flatMap { validateField(model, _) }
  }

  def validateField(model: Model, field: Field): Seq[String] = {
    validateName(field.name) { m => error(model, field, m) }
  }

  def validateUnion(union: Union): Seq[String] = {
    validateName(union.name) { m => error(union, m) } ++ validateDiscriminator(union) ++ union.types.flatMap { validateUnionType(union, _) }
  }

  def validateDiscriminator(union: Union): Seq[String] = {
    union.discriminator match {
      case None => Nil
      case Some(disc) => validateName(disc) { m => error(union, s"discriminator: $m") }
    }
  }

  def validateUnionType(union: Union, unionType: UnionType): Seq[String] = {
    validateName(unionType.`type`) { m => error(union, unionType, m) } ++ validateUnionTypeDiscriminatorValue(union, unionType)
  }

  def validateUnionTypeDiscriminatorValue(union: Union, unionType: UnionType): Seq[String] = {
    unionType.discriminatorValue match {
      case None => Nil
      case Some(dv) => validateName(dv) { m => error(union, unionType, s"discriminator value: $m") }
    }
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.flatMap(validateOperation(resource, _))
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.flatMap(validateParameter(resource, operation, _))
  }

  def validateParameter(resource: Resource, operation: Operation, param: Parameter): Seq[String] = {
    validateName(param.name) { m => error(resource, operation, param, m) }
  }

  def validateName(name: String)(errorF: String => String): Seq[String] = {
    Suggestions.get(name) match {
      case None => Nil
      case Some(s) => Seq(errorF(s"The term '$name' must be replaced by '$s'"))
    }
  }

}
