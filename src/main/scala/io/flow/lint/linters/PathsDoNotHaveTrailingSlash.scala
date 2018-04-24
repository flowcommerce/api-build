package io.flow.lint.linters

import io.apibuilder.spec.v0.models.Service
import io.flow.lint.Linter

case object PathsDoNotHaveTrailingSlash extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.resources.flatMap { resource =>
      resource.operations.
        filter(_.path.endsWith("/")).map { op =>
          error(resource, op, "Path cannot end with '/'")
        }
    }
  }

}
