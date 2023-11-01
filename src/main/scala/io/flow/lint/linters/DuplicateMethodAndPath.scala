package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Operation, Service}
import io.flow.lint.Linter

/** Validates that we do not have duplicate method and paths
  */
case object DuplicateMethodAndPath extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap(_.operations).groupBy(toKey).filter(_._2.length > 1).keys.toList.sorted match {
      case Nil => Nil
      case dups => Seq(s"1 or more operation paths is duplicated: ${dups.mkString(", ")}")
    }
  }

  private[this] def toKey(operation: Operation): String = {
    s"${operation.method} ${operation.path}"
  }
}
