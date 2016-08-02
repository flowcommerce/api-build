package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, ParameterLocation, Resource, Response, Service}
import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

/**
  * We keep all paths in lower case to avoid any issues with case
  * sensitivity.
  */
case object LowerCasePaths extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap { resource =>
      resource.operations.
        filter( op => op.path != op.path.toLowerCase ).map { op =>
          error(resource, op, "Path must be all lower case")
        }
    }
  }

}
