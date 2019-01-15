package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Operation, Parameter, Resource, Service}

/**
  *  We have decided to call the same things consistently. This linter
  *  validates common field and parameter names
  */
case object BadNames extends Linter with Helpers {

  private[this] val Data: Map[String, String] = Map(
    "country_of_origin" -> "origin",
    "ip_address" -> "ip",
    "postal_code" -> "postal"
  )

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel) ++ service.resources.flatMap(validateResource)
  }

  def validateModel(model: Model): Seq[String] = {
    model.fields.flatMap { validateField(model, _) }
  }

  def validateField(model: Model, field: Field): Seq[String] = {
    Data.get(field.name) match {
      case None => Nil
      case Some(replacement) => Seq(error(model, field, s"Name must be '$replacement'"))
    }
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.flatMap(validateOperation(resource, _))
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.flatMap(validateParameter(resource, operation, _))
  }

  def validateParameter(resource: Resource, operation: Operation, param: Parameter): Seq[String] = {
    Data.get(param.name) match {
      case None => Nil
      case Some(replacement) => Seq(error(resource, operation, param, s"Name must be '$replacement'"))
    }
  }

}
