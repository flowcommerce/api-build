package io.flow.lint.util

object Expansions {

  /**
    * Accepts a list of field types, returning the list of possible
    * examples.
    * 
    * Examples:
    *   Seq("id", "name") => Nil
    *   Seq("id", "expandable_organization") => Seq("organization")
    */
  def fromFieldTypes(fields: Seq[String]): Seq[String] = {
    fields.flatMap(toName(_))
  }

  private[this] def toName(field: String): Option[String] = {
    val idx = field.lastIndexOf(".")
    val name = if (idx < 0) { field } else { field.substring(idx + 1) }

    // If the name starts with '[' and ends with ']' it represents an array
    // We want to remove both those characters before checking if it starts with expandable
    val formattedName = if (name.startsWith("[") && name.endsWith("]")) name.stripPrefix("[").stripSuffix("]") else name
    formattedName.startsWith("expandable_") match {
      case true => Some(formattedName.replace("expandable_", ""))
      case false => None
    }
  }

}
