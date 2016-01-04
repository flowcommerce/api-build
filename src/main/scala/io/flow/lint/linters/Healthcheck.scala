package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

/**
  * Enforces that each service has a healthcheck method in a standard
  * location.
  */
case object Healthcheck extends Linter with Helpers {

  val Plural = "healthchecks"

  private[this] val Path = "/_internal_/healthcheck"
  private[this] val Datatype = "io.flow.common.v0.models.healthcheck"

  override def validate(service: Service): Seq[String] = {
    service.resources.isEmpty match {
      case true => Nil
      case false => {
        service.resources.find(_.plural == Plural) match {
          case None => Seq(s"Missing resource: $Plural")
          case Some(resource) => validateResource(resource)
        }
      }
    }
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.filter(op => op.method == Method.Get && op.path == Path) match {
      case Nil => Seq(error(resource, s"Missing GET $Path"))
      case op :: Nil => validateOperation(resource, op)
      case multiple => Seq(error(resource, s"Multiple operations found at GET $Path"))
    }
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.filter(p => p.required && p.default.isEmpty) match {
      case Nil => validateResponses(resource, operation)
      case _ => Seq(error(resource, operation, "must not have any required parameters"))
    }
  }

  def validateResponses(resource: Resource, operation: Operation): Seq[String] = {
    operation.responses match {
      case Nil => Seq(error(resource, operation, "must have at least 1 response"))
      case r :: Nil => validateResponse(resource, operation, r)
      case multiple => Seq(error(resource, operation, "must have exactly 1 response"))
    }
  }

  def validateResponse(resource: Resource, operation: Operation, response: Response): Seq[String] = {
    val statusErrors = response.code match {
      case ResponseCodeInt(200) | ResponseCodeOption.Default => {
        Nil
      }
      case ResponseCodeInt(n) => {
        Seq(error(resource, operation, s"reponse must return HTTP 200 and not HTTP $n"))
      }
      case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
        Seq(error(resource, operation, "reponse must return HTTP 200"))
      }
    }

    val responseTypeErrors = response.`type` == Datatype match {
      case true => Nil
      case false => {
        Seq(error(resource, operation, s"response must be of type ${Datatype} and not ${response.`type`}"))
      }
    }

    statusErrors ++ responseTypeErrors
  }

}
