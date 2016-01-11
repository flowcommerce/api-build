package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Operation, Parameter, Resource, Service}

/**
  * parameters named:
  * 
  *   id, limit, offset, sort, expand
  * 
  * should not have descriptions. This enables us to generate
  * consistent documenetation without worrying about whether a
  * particular description adds anything useful.
  */
case object CommonParametersHaveNoDescriptions extends Linter with Helpers {

  val NamesWithNoDescriptions = Seq("id", "limit", "offset", "sort", "expand")

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap(validateResource(service, _))
  }

  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.flatMap(validateOperation(service, resource, _))
  }

  def validateOperation(service: Service, resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.flatMap(validateParameterDescription(service, resource, operation, _))
  }

  def validateParameterDescription(service: Service, resource: Resource, operation: Operation, parameter: Parameter): Seq[String] = {
    parameter.description match {
      case None => {
        Nil
      }
      case Some(desc) => {
        NamesWithNoDescriptions.contains(parameter.name) match {
          case false => {
            Nil
          }
          case true => {
            Seq(error(resource, operation, parameter, "Must not have a description"))
          }
        }
      }
    }
  }

}
