package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Service, Union}

/**
  * Validates that every union type must have a discriminator with
  * value 'discriminator'
  */
case object UnionTypesHaveCommonDiscriminator extends Linter with Helpers {

  private[this] val StandardName = "discriminator"
  private[this] val ErrorName = "code"

  override def validate(service: Service): Seq[String] = {
    service.unions.flatMap(validateUnion(_))
  }

  def validateUnion(union: Union): Seq[String] = {
    val target = union.name.endsWith("_error") match {
      case true => ErrorName
      case false => union.name == "localized_price" match {
        case true => "key" // hack for now for catalog.localized_price...
        case false => StandardName
      }
    }

    union.discriminator match {
      case None => {
        Seq(error(union, s"Must have a discriminator with value '$target'"))
      }
      case Some(actual) => {
        (actual == target) match {
          case true => Nil
          case false => Seq(error(union, s"Discriminator must have value '$target' and not '$actual'"))
        }
      }
    }
  }

}
