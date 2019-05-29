package io.flow.oneapi

import io.apibuilder.spec.v0.models.Service

case class ApibuilderType(
  qualified: String,
  name: String
)


object ApibuilderType {

  def apply(service: Service, name: String): Option[ApibuilderType] = {
    val join = service.enums.find(_.name == name) match {
      case Some(_) => Some("enums")
      case None => service.models.find(_.name == name) match {
        case Some(_) => Some("models")
        case None => service.unions.find(_.name == name) match {
          case None => None
          case Some(_) => Some("unions")
        }
      }
    }
    join.map { j =>
      ApibuilderType(
        qualified = s"${service.namespace}.$j.$name",
        name = name
      )
    }
  }
}


