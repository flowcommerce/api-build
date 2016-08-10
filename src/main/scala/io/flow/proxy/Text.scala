package io.flow.proxy

object Text {

  implicit class Indentable(s: String) {
    def indent: String = indent(2)
    def indent(width: Int): String = {
      s.split("\n").map { value =>
        if (value.trim == "") {
          ""
        } else {
          (" " * width) + value
        }
      }.mkString("\n")
    }
  }

  /**
    * Return the name up to the last dash. E.g. "currency-internal" ->
    * Some("currency")
    */
  def shortenName(name: String): Option[String] = {
    val i = name.lastIndexOf("-")
    if (i > 0) {
      name.substring(0, i) match {
        case "" => None
        case v => Some(v)
      }
    } else {
      None
    }
  }

  
}
