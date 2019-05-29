package io.flow.oneapi

import io.apibuilder.spec.v0.models.{Import, Service}

case class ApibuilderType(
  qualified: String,
  name: String
)


object ApibuilderType {

  def allDefinedInService(service: Service, imp: Import): Boolean = {
    allDefinedInService(
      service,
      imp.models ++ imp.unions ++ imp.enums
    )
  }

  def allDefinedInService(service: Service, names: Seq[String]): Boolean = {
    names.forall { n =>
      definedInService(service, n)
    }
  }

  def definedInService(service: Service, name: String): Boolean = {
    ApibuilderType(service, name).isDefined
  }

  def apply(service: Service, name: String): Option[ApibuilderType] = {
    assert(
      name.indexOf(".") < 0,
      s"Invalid name[$name] - '.' not allowed"
    )
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
    println(s" apply: $name join[$join]")
    join.map { j =>
      ApibuilderType(
        qualified = s"${service.namespace}.$j.$name",
        name = name
      )
    }
  }
}


