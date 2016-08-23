package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Operation, Resource, Service}

/**
  * A rule that ensures a sort attribute exists for every operation that has a sort parameter.
  */
case object SortAttribute extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.resources
      .flatMap { resource: Resource =>
      resource.operations
        .filter(_.parameters.exists(_.name == "sort"))
        .filter(op => !ignored(op.attributes, "sort"))
        .flatMap(validateOperationHasSortAttribute(resource, _))
    }
  }

  def validateOperationHasSortAttribute(resource: Resource, operation: Operation): Seq[String] = {
    operation.attributes.find(_.name == "sort") match {
      case None => Seq(error(resource, operation, "Missing attribute named sort"))
      case Some(_) => Nil
    }
  }
}
