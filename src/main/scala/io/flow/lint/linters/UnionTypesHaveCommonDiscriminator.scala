package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Service, Union}

/**
  * Validates that every union type must have a discriminator with
  * value 'discriminator'
  */
case object UnionTypesHaveCommonDiscriminator extends Linter with Helpers {

  private[this] val DesiredValue = "discriminator"

  override def validate(service: Service): Seq[String] = {
    service.unions.flatMap(validateUnion(_))
  }

  def validateUnion(union: Union): Seq[String] = {
    union.discriminator match {
      case None => {
        Seq(error(union, s"Must have a discriminator with value '$DesiredValue'"))
      }
      case Some(actual) => {
        (actual == DesiredValue) match {
          case true => Nil
          case false => Seq(error(union, s"Discriminator must have value '$DesiredValue' and not $actual"))
        }
      }
    }
  }

}

