package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Service, Union}

/**
  * Validates that every union type must have a discriminator with
  * value 'discriminator'
  */
case object UnionTypesHaveCommonDiscriminator extends Linter with Helpers {

  private val ValidNames = Seq("discriminator", "type", "code")

  override def validate(service: Service): Seq[String] = {
    service.unions.flatMap(validateUnion)
  }

  def validateUnion(union: Union): Seq[String] = {
    lazy val expected = expectedDiscriminator(union.name)
    lazy val expectedStr = expected.map("'" + _ + "'").mkString("(", ", ", ")")

    union.discriminator match {
      case None => Seq(error(union, s"Must have a discriminator with value one of $expectedStr"))
      case Some(actual) if expected.contains(actual) => Nil
      case Some(actual) => Seq(error(union, s"Discriminator must have value one of $expectedStr and not '$actual'"))
    }
  }

  private[this] def expectedDiscriminator(typeName: String): Seq[String] =
    if (isError(typeName))
      Seq("code")
    else if (typeName == "localized_price")
      // one time hack - do not repeat
      Seq("key")
    else
      ValidNames
}
