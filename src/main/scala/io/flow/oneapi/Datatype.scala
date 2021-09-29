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
  * names are expected to be local
  */
case class TextDatatypeParser(localTypes: Set[String]) {
  import TextDatatype._

  def parse(value: String): Seq[TextDatatype] = {
    internalParse(value).map {
      case s: TextDatatype.Singleton => TextDatatype.Singleton(
        maybeStripNamespace(s.name)
      )
      case other => other
    }
  }

  private[this] def internalParse(value: String): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ parse(t)
      case MapRx(t) => Seq(TextDatatype.Map) ++ parse(t)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton("string"))
      case _ => Seq(TextDatatype.Singleton(value))
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

  def toString(parts: Seq[TextDatatype]): String = {
    parts.toList match {
      case Nil => ""
      case Singleton(name) :: Nil => name

      case one :: rest => {
        one match {
          case List => "[" + toString(rest) + "]"
          case Map => "map[" + toString(rest) + "]"
          case Singleton(_) => sys.error("Did not expect singleton here")
        }
        
      }
    }
  }

  def maybeStripNamespace(value: String): String = {
    if (localTypes.contains(value)) {
      value.split("\\.").last
    } else {
      value
    }
  }

}
