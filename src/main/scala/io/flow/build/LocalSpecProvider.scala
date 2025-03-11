package io.flow.build

import io.apibuilder.spec.v0.models.Service

import java.io.File
import scala.concurrent.ExecutionContext

class LocalSpecProvider extends SpecProvider {

  override def downloadServices(distinct: Seq[Application])(implicit
    ec: ExecutionContext,
  ): Either[Seq[String], Seq[Service]] = {
    val maybeServices = distinct.map(loadLocalSpec)
    val lefts = maybeServices.collect { case Left(err) => err }
    val rights = maybeServices.collect { case Right(service) => service }
    if (lefts.isEmpty) Right(rights) else Left(lefts)
  }

  private def loadLocalSpec(a: Application): Either[String, Service] = {
    // Will ignore a.organization ("flow").
    val candidateFiles = Seq("spec", "spec-event").map(dir => s"$dir/${a.application}")
    val maybeServices = candidateFiles.map(tryLoad)
    val errors = maybeServices.collect { case Left(err) => err }
    val services = maybeServices.collect { case Right(service) => service }
    services.headOption.map(Right(_)).getOrElse(Left(errors.mkString(", ")))
  }

  private def tryLoad(pathname: String): Either[String, Service] = {
    // No idea how to go from a JSON file to a Service instance.
    Left(s"not implemented loading from $pathname")
  }
}
