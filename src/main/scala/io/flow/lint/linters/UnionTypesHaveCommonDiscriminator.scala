package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Service, Union}

/**
  * Validates that every union type must have a discriminator with
  * value 'discriminator'
  */
case object UnionTypesHaveCommonDiscriminator extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.unions.flatMap(validateUnion)
  }

  def validateUnion(union: Union): Seq[String] = {
    val expected = expectedDiscriminator(union.name)

    union.discriminator match {
      case None => Seq(error(union, s"Must have a discriminator with value '$expected'"))
      case Some(actual) if actual == expected => Nil
      case Some(actual) => Seq(error(union, s"Discriminator must have value '$expected' and not '$actual'"))
    }
  }

  private[this] def expectedDiscriminator(typeName: String): String = {
    if (isError(typeName)) {
      "code"
    } else if (typeName == "localized_price") {
      // one time hack - do not repeat
      "hack"
    } else {
      "discriminator"
    }
  }
}
