package io.flow.oneapi

import scala.util.matching.Regex

sealed trait TextDatatype

object TextDatatype {

  case object List extends TextDatatype
  case object Map extends TextDatatype
  case class Singleton(name: String) extends TextDatatype

  val ListRx: Regex = "^\\[(.*)\\]$".r
  val MapRx: Regex = "^map\\[(.*)\\]$".r
  val MapDefaultRx: Regex = "^map$".r

}

/**
  * Parses a text datatype, removing specific namespaces as those
  * names are expected to be local.
  */
object TextDatatypeParser {
  import TextDatatype._

  def parse(localTypes: Set[String], value: String): Seq[TextDatatype] = {
    internalParse(value).map {
      case s: TextDatatype.Singleton => TextDatatype.Singleton(
        maybeStripNamespace(localTypes, s.name)
      )
      case other => other
    }
  }

  def toNamespace(value: String): Option[String] = {
    internalParse(value).lastOption.flatMap {
      case s: TextDatatype.Singleton => {
        val parts = s.name.split("\\.")
        if (parts.length > 2) {
          // eg. io.flow.search.v0.models.foo => namespace is "io.flow.search.v0"
          Some(parts.dropRight(2).mkString("."))
        } else {
          None
        }
      }
      case _ => None
    }
  }

  private[this] def internalParse(value: String): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ internalParse(t)
      case MapRx(t) => Seq(TextDatatype.Map) ++ internalParse(t)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton("string"))
      case _ => Seq(TextDatatype.Singleton(value))
    }
  }

  def toTypeLabel(parts: Seq[TextDatatype]): String = {
    parts.toList match {
      case Nil => ""
      case one :: rest => {
        one match {
          case List => "[" + toTypeLabel(rest) + "]"
          case Map => "map[" + toTypeLabel(rest) + "]"
          case Singleton(name) => name
        }
      }
    }
  }

  def maybeStripNamespace(localTypes: Set[String], value: String): String = {
    if (localTypes.contains(value)) {
      value.split("\\.").last
    } else {
      value
    }
  }

}
