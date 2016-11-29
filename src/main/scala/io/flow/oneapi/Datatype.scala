package io.flow.oneapi

import io.flow.build.BuildType

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
  * Parses a text datatype, removing any namespaces as all names are
  * expected to be local
  *
  * @param namespace - service namespace
  *
  */
case class TextDatatypeParser(namespace: Option[String] = None) {
  import TextDatatype._

  def parse(value: String): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ parse(t)
      case MapRx(t) => Seq(TextDatatype.Map) ++ parse(t)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton("string"))
      case _ => {
        val prefix = namespace.exists(ns => ns.contains("internal")) match {
          case true => Some("internal")
          case false => None
        }
        Seq(TextDatatype.Singleton(Seq(prefix.getOrElse(""), value.split ("\\.").last).mkString("_")))
      }
    }
  }

  def parseString(value: String): String = {
    toString(parse(value))
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
