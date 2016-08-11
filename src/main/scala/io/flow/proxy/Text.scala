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

  def stripSuffix(value: String, suffix: String): String = {
    value.endsWith(suffix) match {
      case true => value.substring(0, value.length - suffix.length)
      case false => value
    }
  }

  
}
