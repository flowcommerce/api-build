package io.flow.oneapi

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
          case Singleton(_) => sys.error("Did not expect singleton here")
        }
        
      }
    }
  }

}
