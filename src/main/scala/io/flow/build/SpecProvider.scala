package io.flow.build

import io.apibuilder.spec.v0.models.Service

trait SpecProvider {

  def downloadServices(distinct: Seq[Application])(implicit
    ec: scala.concurrent.ExecutionContext,
  ): Either[Seq[String], Seq[Service]]
}
