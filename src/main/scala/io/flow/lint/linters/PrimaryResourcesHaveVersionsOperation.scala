package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Method, Operation, Resource, Service}

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
    val data: Seq[Data] = nonHealthcheckResources(service).
      flatMap { resource =>
        resource.operations.
          filter(op => !ignored(op.attributes, "versions")).
          filter(_.method == Method.Get).
          filter(returnsArray).
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
          if (t.endsWith("_version")) {
            None
          } else {
            Some(error(item.resource, item.operation, s"2xx response type should be '${t}_version' and not $t"))
          }
        }
      }
    }

    val paths = data.map(_.operation.path)
    val nonVersionsOperationErrors = data.filter(!_.operation.path.endsWith("/versions")).flatMap { item =>
      val versionPath = if (item.operation.path == "/") { "/versions"} else { s"${item.operation.path}/versions" }
      if (paths.contains(versionPath)) {
        responseType(item.operation) match {
          case None => Some(error(item.resource, item.operation, s"Missing a 2xx response"))
          case Some(_) => Nil
        }
      } else {
        /* If operation has attribute with name 'non-crud', no /versions route is required.
         * The resource is most likely manipulating/aggregating data rather than CRUD
         */
        if (item.operation.attributes.exists(_.name == "non-crud"))
          Nil
        else
          Some(error(item.resource, item.operation, s"Missing versions operation at path $versionPath"))
      }
    }

    versionsOperationErrors ++ nonVersionsOperationErrors
  }

}
