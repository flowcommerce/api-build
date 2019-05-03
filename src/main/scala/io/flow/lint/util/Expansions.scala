package io.flow.lint.util

import io.flow.lint.linters.Helpers

object Expansions extends Helpers {

  /**
    * Accepts a list of field types, returning the list of possible
    * examples.
    * 
    * Examples:
    *   Seq("id", "name") => Nil
    *   Seq("id", "expandable_organization") => Seq("organization")
    */
  def fromFieldTypes(fields: Seq[String]): Seq[String] = {
    fields.flatMap(toName)
  }

  private[this] def toName(field: String): Option[String] = {
    val idx = field.lastIndexOf(".")
    val name = if (idx < 0) { baseType(field) } else { baseType(field.substring(idx + 1)) }
    if (name.startsWith("expandable_")) {
      Some(name.replace("expandable_", ""))
    } else {
      None
    }
  }

}
