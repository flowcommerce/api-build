package io.flow.oneapi

import io.flow.oneapi.Util.namespaceTypeName

sealed trait TextDatatype

object TextDatatype {

  case object List extends TextDatatype
  case object Map extends TextDatatype
  case class Singleton(name: String) extends TextDatatype

  val ListRx = "^\\[(.*)\\]$".r
  val MapRx = "^map\\[(.*)\\]$".r
  val MapDefaultRx = "^map$".r
}

/**
  * Parses a text datatype, removing specific namespaces as those
  * names are expected to be local
  */
case class TextDatatypeParser() {
  import TextDatatype._

  def parse(value: String, dups: Seq[String] = Seq(), namespace: Option[String] = None): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ parse(t, dups, namespace)
      case MapRx(t) => Seq(TextDatatype.Map) ++ parse(t, dups, namespace)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton("string"))
      case _ =>
        val name =
          if (value.contains('.'))
            value
          else
            namespace.map(ns => s"$ns.types.$value").getOrElse(value)

        val parts = name.split('.')

        val dedupedName =
          if (dups.contains(parts.last)) {
            // .init.init because we want to remove the .models, .enums, or .unions
            println(namespace, parts.toList)
            namespaceTypeName(parts.init.init.mkString("."), parts.last)
          } else
            parts.last

        Seq(TextDatatype.Singleton(dedupedName))
    }
  }

  def toString(parts: Seq[TextDatatype]): String = {
    parts.toList match {
      case Nil => ""
      case Singleton(name) :: Nil => name

      case one :: rest => {
        one match {
          case List => "[" + toString(rest) + "]"
          case Map => "map[" + toString(rest) + "]"
          case Singleton(name) => sys.error("Did not expect singleton here")
        }

      }
    }
  }

}
