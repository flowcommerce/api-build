package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, ParameterLocation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

/**
  * Makes sure we have /versions methods for all of our resources.
  * 
  * Find all GET methods that return an array where path does not
  * end in /versions, validate that there is a corresponding operation
  * with the same path + /versions
  */
case object PrimaryResourcesHaveVersionsOperation extends Linter with Helpers {

  private[this] case class Data(
    resource: Resource,
    operation: Operation
  )

  override def validate(service: Service): Seq[String] = {
    val data: Seq[Data] = nonHealthcheckResources(service).flatMap { resource =>
      resource.operations.
        filter(_.method == Method.Get).
        filter(returnsArray(_)).
        map { operation =>
          Data(resource, operation)
        }
    }

    val versionsOperationErrors = data.filter(_.operation.path.endsWith("/versions")).flatMap { item =>
      responseType(item.operation) match {
        case None => {
          Some(error(item.resource, item.operation, s"Missing a 2xx response"))
        }
        case Some(t) => {
          t.endsWith("_version") match {
            case true => None
            case false => Some(error(item.resource, item.operation, s"2xx response type should be '${t}_version' and not $t"))
          }
        }
      }
    }

    val paths = data.map(_.operation.path)
    val nonVersionsOperationErrors = data.filter(!_.operation.path.endsWith("/versions")).flatMap { item =>
      val versionPath = if (item.operation.path == "/") { "/versions"} else { s"${item.operation.path}/versions" }
      paths.contains(versionPath) match {
        case true => {
          responseType(item.operation) match {
            case None => Some(error(item.resource, item.operation, s"Missing a 2xx response"))
            case Some(_) => Nil
          }
        }
        case false => {
          // if successful response type (2xx) references a model from another schema (i.e. [io.flow.example.v0.models.object]), no /versions endpoint is required
          // the resource is most likely manipulating/aggregating data rather than CRUD
          if (!responseType(item.operation).getOrElse("").contains("."))
            Some(error(item.resource, item.operation, s"Missing versions operation at path $versionPath"))
          else
            Nil
        }
      }
    }

    versionsOperationErrors ++ nonVersionsOperationErrors
  }

}
