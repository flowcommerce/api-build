package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Service, Union}

/**
  * Validates that we are consistent with our expansion union types, e.g.:
  * 
  *  "expandable_organization": {
  *    "discriminator": "discriminator",
  *    "types": [
  *      { "type": "organization" },
  *      { "type": "organization_reference" }
  *    ]
  *  }
  * 
  * We validate that any union named expandable_xxx has exactly two
  * types: xxx and xxx_reference. Note that the reason we force the
  * reference last is that when we use parser combinators, we look in
  * order for the first matching type. Having the types in this order
  * means the organization will match before the reference (otherwise
  * we would always end up matching on the reference).
  */
case object ExpandableUnionsAreConsistent extends Linter with Helpers {

  private[this] val Pattern = """^expandable_(.+)$""".r

  override def validate(service: Service): Seq[String] = {
    service.unions.flatMap { u => validateUnion(service, u) }
  }

  def validateUnion(service: Service, union: Union): Seq[String] = {
    union.name match {
      case Pattern(name) => {
        service.unions.find(_.name == name) match {
          case None => validateUnionTypes(union, Seq(name, s"${name}_reference"))
          case Some(u) => validateUnionTypes(union, u.types.map(_.`type`) ++ Seq(s"${name}_reference"))
        }
      }
      case _ => {
        Nil
      }
    }
  }

  def validateUnionTypes(union: Union, types: Seq[String]): Seq[String] = {
    val names = union.types.map(_.`type`)
    if (names == types) {
      Nil
    } else {
      names.toList match {
        case Nil => {
          Seq(error(union, s"Types for this expandable union must be ${types.mkString(", ")}"))
        }
        case _ => {
          Seq(error(union, s"Types for this expandable union must be ${types.mkString(", ")} and not ${names.mkString(", ")}"))
        }
      }
    }
  }
}
