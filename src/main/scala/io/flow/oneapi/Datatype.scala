package io.flow.oneapi

import io.apibuilder.spec.v0.models.Service
import io.flow.build.BuildType

import scala.util.matching.Regex

sealed trait TextDatatype

object TextDatatype {

  case object List extends TextDatatype

  case object Map extends TextDatatype

  case class Singleton(name: String) extends TextDatatype

  val ListRx: Regex = "^\\[(.*)\\]$".r
  val MapRx: Regex = "^map\\[(.*)\\]$".r
  val MapDefaultRx: Regex = "^map$".r

  def definedInService(service: Service, namespace: String): Boolean = {
    if (namespace.startsWith("io.flow.")) {
      val parts = namespace.split("\\.")
      if (parts.contains("internal") || parts.contains("external") || parts.contains("misc")) {
        false
      } else {
        true
      }
    } else {
      false
    }
  }

}

/**
  * Parses a text datatype, removing specific namespaces as those
  * names are expected to be local
  */
case class TextDatatypeParser(flowApi: Service, buildType: BuildType) {
  import TextDatatype._

  def parse(value: String): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ parse(t)
      case MapRx(t) => Seq(TextDatatype.Map) ++ parse(t)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton("string"))
      case _ => Seq(TextDatatype.Singleton(
        maybeStripNamespace(value)
      ))
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
    if (TextDatatype.definedInService(value)) {
      val name = value.split("\\.").last
      buildType match {
        case BuildType.Api => {
          // unqalified as local to the api project
          name
        }
        case _ => {
          ApibuilderType(flowApi, name) match {
            case None => {
              value
            }
            case Some(t) => {
              // rewrites the namespace to io.flow.v0.models.xxx
              println(s" => ${t.qualified}")
              t.qualified
            }
          }
        }
      }
    } else {
      value
    }
  }

}
