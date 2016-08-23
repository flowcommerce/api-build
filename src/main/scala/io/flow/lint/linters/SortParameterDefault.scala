package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Operation, Resource, Service}

/**
  *  for resources w/ sort parameter:
  *    - default to created_at if path ends with /versions
  *    - default to name, -created_at if there is a name field of type string
  *    - otherwise default to -created_at
  */
case object SortParameterDefault extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap(validateResource(service, _))
  }

  def validateResource(service: Service, resource: Resource): Seq[String] = {
    resource.operations.
      filter(r => !ignored(r.attributes, "sort")).
      filter(r => !ignored(r.attributes, "sort_parameter_default")).
      flatMap(validateOperation(service, resource, _))
  }

  def validateOperation(service: Service, resource: Resource, operation: Operation): Seq[String] = {
    operation.parameters.find(_.name == "sort") match {
      case None => {
        Nil
      }
      case Some(sort) => {
        sort.default match {
          case None => {
            Seq(error(resource, operation, "Parameter sort requires a default"))
          }
          case Some(default) => {
            val expected = computeDefaults(service, operation)
            expected.contains(default) match {
              case true => Nil
              case false => {
                Seq(error(resource, operation, s"Parameter sort default expected to be[${expected.mkString(" or ")}] and not[$default]"))
              }
            }
          }
        }
      }
    }
  }

  def computeDefaults(service: Service, operation: Operation): Seq[String] = {
    operation.path.endsWith("/versions") match {
      case true => {
        Seq("journal_timestamp")
      }
      case false => {
        model(service, operation) match {
          case None => {
            Seq("-created_at", "name")
          }
          case Some(model) => {
            model.fields.find(f => f.name == "name" && f.`type` == "string") match {
              case None => Seq("-created_at")
              case Some(_) => Seq("name")
            }
          }
        }
      }
    }
  }

}
