package io.flow.oneapi

import apibuilder.ApiBuilderHelperImpl
import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.{ApiBuilderService, MultiServiceImpl, TypeName}

import java.util.UUID

/**
 * Finds all declared types in a service, iterating through all models,
 * fields, unions, responses, etc.
 */
object AllTypeNames {

  private[this] val defaultNamespace = UUID.randomUUID().toString

  def findNamespaces(service: Service): Set[String] = {
    find(service).flatMap { t =>
      Some(TypeName.parse(t, defaultNamespace = defaultNamespace).namespace).filterNot(_ == defaultNamespace)
    }
  }

  def find(service: Service): Set[String] = {
    val helper = ApiBuilderHelperImpl(MultiServiceImpl(List(ApiBuilderService(service))))

    (
      service.models.flatMap(find) ++
        service.unions.flatMap(find) ++
        service.interfaces.flatMap(find) ++
        service.resources.flatMap(find)
    ).map(helper.baseType(service, _)).toSet
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


