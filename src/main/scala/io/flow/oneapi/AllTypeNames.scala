package io.flow.oneapi

import io.apibuilder.spec.v0.models._

object AllTypeNames {

  def find(service: Service): Set[String] = {
    (
      service.models.flatMap(find) ++
        service.unions.flatMap(find) ++
        service.interfaces.flatMap(find) ++
        service.resources.flatMap(find)
    ).toSet
  }

  private[this] def find(model: Model): Seq[String] = {
    model.fields.flatMap(find)
  }

  private[this] def find(field: Field): Seq[String] = {
    Seq(field.`type`)
  }

  private[this] def find(union: Union): Seq[String] = {
    union.types.flatMap(find)
  }

  private[this] def find(unionType: UnionType): Seq[String] = {
    Seq(unionType.`type`)
  }

  private[this] def find(interface: Interface): Seq[String] = {
    interface.fields.flatMap(find)
  }

  private[this] def find(resource: Resource): Seq[String] = {
    Seq(resource.`type`) ++ resource.operations.flatMap(find)
  }

  private[this] def find(operation: Operation): Seq[String] = {
    operation.body.toSeq.map(_.`type`) ++
      operation.parameters.flatMap(find) ++
      operation.responses.flatMap(find)
  }

  private[this] def find(parameter: Parameter): Seq[String] = {
    Seq(parameter.`type`)
  }

  private[this] def find(response: Response): Seq[String] = {
    Seq(response.`type`) ++ response.headers.getOrElse(Nil).map(_.`type`)
  }
}


