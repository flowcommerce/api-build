package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Operation, Parameter, Resource, Service}
import scala.util.{Failure, Success, Try}

/**
  *  We have decided to call the same things consistently. This linter
  *  validates common field and parameter names
  */
case object BadNames extends Linter with Helpers {

  val Data = Map(
    "country_of_origin" -> "origin",
    "ip_address" -> "ip",
    "postal_code" -> "postal"
  )

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel(service, _)) ++ service.resources.flatMap(validateResource(service, _))
  }

  def validateModel(service: Service, model: Model): Seq[String] = {
    model.fields.flatMap { validateField(model, _) }
  }

  def validateField(model: Model, field: Field): Seq[String] = {
    Data.get(field.name) match {
      case None => Nil
      case Some(replacement) => Seq(error(model, field, s"Name must be '$replacement'"))
    }
  }

  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.flatMap(validateOperation(service, resource, _))
  }

  def validateOperation(service: Service, resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.flatMap(validateParameter(service, resource, operation, _))
  }

  def validateParameter(service: Service, resource: Resource, operation: Operation, param: Parameter): Seq[String] = {
    Data.get(param.name) match {
      case None => Nil
      case Some(replacement) => Seq(error(resource, operation, param, s"Name must be '$replacement'"))
    }
  }

}
