package io.flow.oneapi

sealed trait TextDatatype

/**
  * Parses a text datatype, removing any namespaces as all names are
  * expected to be local
  */
object TextDatatype {

  case object List extends TextDatatype
  case object Map extends TextDatatype
  case class Singleton(name: String) extends TextDatatype

  private val ListRx = "^\\[(.*)\\]$".r
  private val MapRx = "^map\\[(.*)\\]$".r
  private val MapDefaultRx = "^map$".r

  def parse(value: String): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ parse(t)
      case MapRx(t) => Seq(TextDatatype.Map) ++ parse(t)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton("string"))
      case _ => Seq(TextDatatype.Singleton(value.split("\\.").last))
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
