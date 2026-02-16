package io.flow.lint.linters

import io.apibuilder.spec.v0.models._
import io.flow.lint.Linter

case class ParsedType(namespace: Option[String], category: Option[String], name: String)

object ParsedType {
  // Converts
  def apply(input: String): ParsedType = {
    input.lastIndexOf('.') match {
      case -1 => ParsedType(None, None, input)
      case lastDot =>
        val name = input.substring(lastDot + 1)
        val beforeLast = input.substring(0, lastDot)

        beforeLast.lastIndexOf('.') match {
          case -1 =>
            // :
            sys.error(s"Unparseable type: expected either <name> or <namespace>.<category>.<name>, got '$input'")
          case secondLastDot =>
            val namespace = beforeLast.substring(0, secondLastDot)
            val category = beforeLast.substring(secondLastDot + 1)
            ParsedType(Some(namespace), Some(category), name)
        }
    }
  }
}

case class ReferencedType(namespace: String, name: String)

object ReferencedType {
  def from(service: Service): Seq[ReferencedType] = {
    def strip(s: String): String = s.replace('[', ' ').replace(']', ' ').trim

    // Use of imported types are (almost) always fully qualified. e.g. "io.flow.common.v0.unions.Foo"
    // so we can use this to determine when imports are used.  Of course there is one exception, values
    // in the 'annotation' field do not have to be fully qualified.
    //
    // See
    // https://github.com/flowcommerce/api-internal/blob/main/spec-event/paypal-internal-event.json
    // where the only use of the common import is the personal_data annotation.
    (
      service.headers.map(_.`type`) ++:
        service.interfaces.flatMap(types) ++:
        service.models.flatMap(types) ++:
        service.unions.flatMap(types) ++:
        service.resources.flatMap(types)
    )
      .map(strip) // array type [a.b] => a.b
      .distinct
      .map { name =>
        ParsedType(name).namespace match {
          case Some(namespace) => ReferencedType(namespace = namespace, name)
          case None => ReferencedType(namespace = service.namespace, name)
        }
      }
  }

  private def types(m: Model): Seq[String] = m.fields.map(_.`type`)

  private def types(u: Union): Seq[String] = u.types.map(_.`type`)

  private def types(i: Interface): Seq[String] = i.fields.map(_.`type`)

  private def types(r: Resource): Seq[String] = {
    val paramTypes = r.operations.flatMap(_.parameters.map(_.`type`))
    val responseTypes = r.operations.flatMap(_.responses.map(_.`type`))
    Seq(r.`type`) ++: paramTypes ++: responseTypes
  }
}

case object AllImportsAreUsed extends Linter {

  override def validate(service: Service): Seq[String] = {
    val referencedTypes = ReferencedType.from(service)
    val unusedImports = service.imports.filterNot(i => referencedTypes.exists(_.namespace == i.namespace))
    unusedImports.map(unused => s"Unused import: '${unused.uri}'")
  }
}
