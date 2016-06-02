package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models.{Method, Service}

case class OneApi(services: Seq[Service]) {

  println(s"One Api: " + services.map(_.name))

  def process(): Either[Seq[String], Service] = {
    val pathErrors = validatePaths()
    val duplicateRecordErrors = validateRecordNames()

    (pathErrors ++ duplicateRecordErrors).toList match {
      case Nil => {
        Left(Seq("TODO: Finish"))
      }
      case errors => {
        Left(errors)
      }
    }
  }

  def validatePaths(): Seq[String] = {
    val allPaths: Seq[String] = services.flatMap(_.resources).flatMap { r =>
      r.operations.map { op =>
        normalizePath(op.method, op.path)
      }
    }

    dupErrors(allPaths, "path", "paths")    
  }

  def normalizePath(method: Method, path: String): String = {
    val pathParts: Seq[String] = path.toLowerCase.split("/").map { name =>
      if (name.startsWith(":")) {
        // Use a standard name here as this is a pattern - doesn't
        // matter what name the developer actually assigned in terms
        // of how the path will be routed.
        ":var"
      } else {
        name
      }
    }

    method.toString + " " + pathParts.mkString("/")
  }

  def validateRecordNames(): Seq[String] = {
    val names: Seq[String] = services.flatMap(_.models.map(_.name)) ++ services.flatMap(_.unions.map(_.name)) ++ services.flatMap(_.enums.map(_.name))
    dupErrors(names, "name", "names")
  }

  /**
    * Returns an error message if there are duplicate values
    */
  def dupErrors(values: Seq[String], singular: String, plural: String): Seq[String] = {
    dups(values).toList match {
      case Nil => Nil
      case duplicate :: Nil => Seq(s"The following $singular appears more than once: $duplicate")
      case duplicates => Seq(s"The following $plural appears more than once: ${duplicates.sorted.mkString(", ")}")
    }
  }

  def dups(values: Seq[String]): Seq[String] = {
    values.groupBy(_.toLowerCase).filter { _._2.size > 1 }.keys.toSeq
  }

}
