package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Service, Union}

/**
  * Validates that every union type must have a discriminator with
  * value 'discriminator'
  */
case object UnionTypesHaveCommonDiscriminator extends Linter with Helpers {

  private[this] val StandardName = "discriminator"

  override def validate(service: Service): Seq[String] = {
    service.unions.
      filter(op => !ignored(op.attributes, "discriminator")).
      flatMap(validateUnion(_))
  }

  def validateUnion(union: Union): Seq[String] = {
    union.discriminator match {
      case None => Seq(error(union, s"Must have a discriminator with value '$StandardName'"))
      case Some(StandardName) => Nil
      case Some(actual) => Seq(error(union, s"Discriminator must have value '$StandardName' and not '$actual'"))
    }
  }

}
