package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

/**
  * 
  * Response codes:
  * 
  *   GET: 200, 401
  *   POST: 200,401, 422
  *   PUT: 200, 201, 401, 422
  *   DELETE: 204, 401, 404
  * 
  *   - 204 - verify type: unit
  *   - 401 - verify type: unit, description: "unauthorized request"
  *   - 422 - verify type: "[io.flow.common.v0.models.error]"
  */
case object StandardResponse extends Linter with Helpers {

  val RequiredResponseCodes: Map[Method, Seq[Int]] = Map(
    Method.Get -> Seq(200, 401),
    Method.Patch -> Seq(200, 401, 404, 422),
    Method.Post -> Seq(200, 401, 422),
    Method.Put -> Seq(200, 201, 401, 422),
    Method.Delete -> Seq(204, 401, 404),
    Method.Head -> Nil,
    Method.Connect -> Nil,
    Method.Options -> Nil,
    Method.Trace -> Nil
  )

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).flatMap(validateResource(_))
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.flatMap(validateOperation(resource, _))
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    validateResponses(resource, operation) ++ operation.responses.flatMap(validateResponse(resource, operation, _))
  }

  def validateResponses(resource: Resource, operation: Operation): Seq[String] = {
    val actualCodes = operation.responses.flatMap { r =>
      r.code match {
        case ResponseCodeInt(n) => Some(n)
        case ResponseCodeOption.Default => Some(200)
        case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => None
      }
    }.sorted

    RequiredResponseCodes.get(operation.method).map(_.sorted) match {
      case None => {
        Seq(error(resource, operation, s"Missing documentation for required response codes for method[${operation.method}]"))
      }
      case Some(expected) => {
        expected.filter(code => !actualCodes.contains(code)) match {
          case Nil => Nil
          case missing => Seq(error(resource, operation, s"Missing response codes: " + missing.mkString(", ")))
        }
      }
    }
  }

  def validateResponse(resource: Resource, operation: Operation, response: Response): Seq[String] = {
    response.code match {
      case ResponseCodeInt(200) | ResponseCodeOption.Default => {
        Nil
      }
      case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
        Seq(error(resource, operation, "Must document a valid return code"))
      }
      case ResponseCodeInt(n) => {
        n match {
          case 204 => check(resource, operation, response, "unit")
          case 401 => check(resource, operation, response, "unit", description = Some("Unauthorized"))
          case 422 => check(resource, operation, response, "[io.flow.common.v0.models.error]")
          case _ => Nil
        }
      }
    }
  }

  def check(
    resource: Resource,
    operation: Operation,
    response: Response,
    datatype: String,
    description: Option[String] = None
  ): Seq[String] = {
    val responseTypeErrors = response.`type` == datatype match {
      case true => Nil
      case false => {
        Seq(error(resource, operation, response, s"response must be of type ${datatype} and not ${response.`type`}"))
      }
    }

    val descriptionErrors = description match {
      case None => Nil
      case Some(target) => {
        response.description match {
          case None => Seq("description is required")
          case Some(actual) => {
            actual == target match {
              case true => Nil
              case false => {
                Seq(error(resource, operation, response, s"description must be[$target] and not[$actual]"))
              }
            }
          }
        }
      }
    }

    responseTypeErrors ++ descriptionErrors
  }
}
