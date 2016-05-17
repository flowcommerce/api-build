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

  // hack for now
  private[this] val ExcludedApplications = Seq("location", "reference", "search")

  private[this] case class Data(
    resource: Resource,
    operation: Operation
  )

  override def validate(service: Service): Seq[String] = {
    ExcludedApplications.contains(service.name.toLowerCase) match {
      case true => Nil
      case false => doValidate(service)
    }
  }

  private[this] def doValidate(service: Service): Seq[String] = {
    val data: Seq[Data] = nonHealthcheckResources(service).flatMap { resource =>
      resource.operations.
        filter(_.method == Method.Get).
        filter(returnsArray(_)).
        map { operation =>
          Data(resource, operation)
        }
    }

    val versionsOperationErrors = data.filter(_.operation.path.endsWith("/versions")).flatMap { item =>
      responseType(service, item.operation) match {
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
          responseType(service, item.operation) match {
            case None => Some(error(item.resource, item.operation, s"Missing a 2xx response"))
            case Some(_) => Nil
          }
        }
        case false => {
          /** If operation has attribute with name 'non-crud', no /versions route is required.
            * The resource is most likely manipulating/aggregating data rather than CRUD
            **/
          if (item.operation.attributes.exists(_.name == "non-crud"))
            Nil
          else
            Some(error(item.resource, item.operation, s"Missing versions operation at path $versionPath"))
        }
      }
    }

    versionsOperationErrors ++ nonVersionsOperationErrors
  }

}
