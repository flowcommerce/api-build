package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Method, Operation, Resource, Response, Service}
import io.apibuilder.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

/** Response codes:
  *
  * GET: 200, 401 POST: 200,401, 422 PUT: 401, 422 DELETE: 204, 401, 404
  *
  *   - 204 - verify type: unit
  *   - 401 - verify type: unit, description: "unauthorized request"
  */
case object StandardResponse extends Linter with Helpers {

  val RequiredResponseCodes: Map[Method, Seq[Int]] = Map(
    Method.Get -> Seq(200, 401),
    Method.Patch -> Seq(401, 404, 422),
    Method.Post -> Seq(401, 422),
    Method.Put -> Seq(401, 422),
    Method.Delete -> Seq(401, 404),
    Method.Head -> Nil,
    Method.Connect -> Nil,
    Method.Options -> Nil,
    Method.Trace -> Nil,
  )

  override def validate(service: Service): Seq[String] = {
    nonHealthcheckResources(service).flatMap(validateResource(_))
  }

  def validateResource(resource: Resource): Seq[String] = {
    resource.operations.filter(op => !ignored(op.attributes, "response_codes")).flatMap(validateOperation(resource, _))
  }

  def validateOperation(resource: Resource, operation: Operation): Seq[String] = {
    validateResponses(resource, operation) ++
      validateStandardResponsesHaveNoDescription(resource, operation) ++
      operation.responses.flatMap(validateResponse(resource, operation, _))
  }

  def validateStandardResponsesHaveNoDescription(resource: Resource, operation: Operation): Seq[String] = {
    RequiredResponseCodes(operation.method).flatMap { code =>
      operation.responses.find(_.code == ResponseCodeInt(code)) match {
        case None => Nil
        case Some(response) => {
          response.description match {
            case None => Nil
            case Some(_) =>
              Seq(
                error(
                  resource,
                  operation,
                  response,
                  "Must not have a description as this is a globally standard response",
                ),
              )
          }
        }
      }
    }
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
        Seq(
          error(
            resource,
            operation,
            s"Missing documentation for required response codes for method[${operation.method}]",
          ),
        )
      }
      case Some(expected) => {
        expected.filter(code => !actualCodes.contains(code)) match {
          case Nil => Nil
          case missing if missing == Seq(200) && actualCodes.contains(302) => Nil // Treat 302 as success
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
          case 204 if pathIdentifiesResource(operation.path) => compare(resource, operation, response, "unit")
          case 401 => compare(resource, operation, response, "unit")
          case 422 => compare(resource, operation, response, "*_error")
          case _ => Nil
        }
      }
    }
  }

  /** True if last path component is a variable. Used to differentiate between:
    *
    * DELETE /shopify/carts/:id
    *
    * DELETE /shopify/carts/:id/promo
    *
    * where in the second example we want to allow HTTP 200 response (resource itself was not deleted)
    */
  def pathIdentifiesResource(path: String): Boolean = {
    path.split("/").lastOption.getOrElse("").startsWith(":")
  }

  def compare(
    resource: Resource,
    operation: Operation,
    response: Response,
    datatype: String,
  ): Seq[String] = {
    typeMatches(response.`type`, datatype) match {
      case true => {
        Nil
      }
      case false => {
        Seq(error(resource, operation, response, s"response must be of type ${datatype} and not ${response.`type`}"))
      }
    }
  }

  def typeMatches(
    typ: String,
    pattern: String,
  ): Boolean = {
    typ == pattern match {
      case true => true
      case false => pattern.startsWith("*") && typ.endsWith(pattern.drop(1))
    }
  }
}
