package io.flow.proxy

object Text {

  def indent(s: String, width: Int = 2): String = {
    s.split("\n")
      .map { value =>
        if (value.trim == "") {
          ""
        } else {
          (" " * width) + value
        }
      }
      .mkString("\n")
  }

  def stripSuffix(value: String, suffix: String): String = {
    if (value.endsWith(suffix)) {
      value.substring(0, value.length - suffix.length)
    } else {
      value
    }
  }

}
