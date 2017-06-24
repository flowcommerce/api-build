package io.flow.lint

import io.apibuilder.spec.v0.models.Service

trait Linter {

  /**
    * Validates that this service, returning a list of
    * errors. Returning an empty list indicates the service is valid.
    */
  def validate(service: Service): Seq[String]

}
