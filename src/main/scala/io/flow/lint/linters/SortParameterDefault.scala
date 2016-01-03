package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Resource, Service}

/**
  *  for resources w/ sort parameter:
  *    - default to lower(name), created_at if there is a name field
  *    - otherwise default to created_at
  */
case object SortParameterDefault extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap(validateResource(_))
  }

  def validateResource(resource: Resource): Seq[String] = {
    Nil
  }

}
