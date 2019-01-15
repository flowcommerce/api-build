package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Operation, Parameter, Resource, Service}

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

  val NamesWithNoDescriptions: Seq[String] = Seq("id", "limit", "offset", "sort", "expand")

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap(validateResource)
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.
      filter(op => !ignored(op.attributes, "common_parameters_have_no_description")).
      flatMap(validateOperation(resource, _))
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.flatMap(validateParameterDescription(resource, operation, _))
  }

  def validateParameterDescription(resource: Resource, operation: Operation, parameter: Parameter): Seq[String] = {
    parameter.description match {
      case None => {
        Nil
      }
      case Some(_) => {
        if (NamesWithNoDescriptions.contains(parameter.name)) {
          Seq(error(resource, operation, parameter, "Must not have a description"))
        } else {
          Nil
        }
      }
    }
  }

}
