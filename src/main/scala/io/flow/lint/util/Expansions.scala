package io.flow.lint.util

object Expansions {

  def fromFields(fields: Seq[String]): Seq[String] = {
    fields.flatMap(toName(_))
  }

  private[this] def toName(field: String): Option[String] = {
    val idx = field.lastIndexOf(".")
    val name = if (idx < 0) { field } else { field.substring(idx + 1) }
    name.startsWith("expandable_") match {
      case true => Some(name.replace("expandable_", ""))
      case false => None
    }
  }

}
