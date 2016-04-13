package io.flow.lint.linters

import io.flow.lint.Linter
import io.flow.lint.util.Expansions
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Resource, Service}

/**
  * Enforce that for models with expansion where the return type is
  * expandable, the get/:id method has the expand parameter
  */
case object GetByIdIsExpandable extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).flatMap { resource =>
      resource.operations.
        filter(_.method == Method.Get).
        filter(returnsExpandableType(service, _)).
        flatMap {
          validateOperationHasExpandParameter(resource, _)
        }
    }
  }

  def validateOperationHasExpandParameter(resource: Resource, op: Operation): Seq[String] = {
    op.parameters.find(_.name == "expand") match {
      case None => {
        Seq(error(resource, op, "Missing parameter named expand"))
      }
      case Some(_) => {
        Nil
      }
    }
  }

  def returnsExpandableType(service: Service, op: Operation): Boolean = {
    responseType(op) match {
      case None => false
      case Some(t) => {
        Expansions.fromFieldTypes(Seq(t)) match {
          case Nil => false
          case _ => true
        }
      }
    }
  }

}
