package io.flow.stream

object ApiBuilderUtils {
  def toClassName(name: String, quoteKeywords: Boolean = true): String = {
    val baseName = safeName(
      if (name == name.toUpperCase) {
        initCap(splitIntoWords(name).map(_.toLowerCase)).mkString("")
      } else {
        snakeToCamelCase(name).capitalize
      },
    )

    if (quoteKeywords) {
      quoteNameIfKeyword(baseName)
    } else {
      baseName
    }
  }

  def toPackageName(namespace: String, quoteKeywords: Boolean = true): String = {
    namespace.split("\\.").map(s => if (quoteKeywords) quoteNameIfKeyword(s) else s).mkString(".") + ".models"
  }

  private[this] val RemoveUnsafeCharacters = """([^0-9a-zA-Z\-\_])""".r

  private def safeName(name: String): String = {
    RemoveUnsafeCharacters.replaceAllIn(name, _ => "").replaceAll("\\.", "_").replaceAll("\\_+", "_").trim
  }

  private def initCap(parts: Seq[String]): String = {
    parts.map(s => s.capitalize).mkString("")
  }

  private[this] val WordDelimiterRx = "_|\\-|\\.|:|/| ".r

  private def splitIntoWords(value: String): Seq[String] = {
    WordDelimiterRx.split(camelCaseToUnderscore(value)).toSeq.map(_.trim).filter(!_.isEmpty)
  }

  private[this] val Capitals = """([A-Z])""".r

  private def camelCaseToUnderscore(phrase: String): String = {
    if (phrase == phrase.toUpperCase) {
      phrase.toLowerCase
    } else {
      val word = Capitals.replaceAllIn(phrase, m => s"_${m}").trim
      if (word.startsWith("_")) {
        word.slice(1, word.length)
      } else {
        word
      }
    }
  }

  private def snakeToCamelCase(value: String): String = {
    splitIntoWords(value).toList match {
      case Nil => ""
      case part :: rest => part + initCap(rest)
    }
  }

  private[this] val ReservedWords = Seq(
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
  ).toSet

  private def quoteNameIfKeyword(name: String): String = {
    def isKeyword(value: String): Boolean = {
      ReservedWords.contains(value)
    }
    def needsQuoting(name: String): Boolean = {
      name.indexOf("[") >= 0
    }
    if (isKeyword(name) || needsQuoting(name)) {
      "`" + name + "`"
    } else {
      name
    }
  }
}
