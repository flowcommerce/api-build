package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._

case class Lint(
  linters: Seq[Linter] = Lint.All
) {
  
  def validate(service: Service): Seq[String] = {
    linters.map(_.validate(service)).flatten
  }

}

object Lint {

  val All = Seq(
    linters.CommonParametersHaveNoDescriptions,
    linters.Get,
    linters.Healthcheck,
    linters.ModelsWithOrganizationField,
    linters.PrimaryResourcesHaveVersionsOperation,
    linters.SortParameterDefault,
    linters.StandardResponse,
    linters.UnionTypesHaveCommonDiscriminator,
    linters.VersionModels
  )

  def fromFile(path: String): Seq[String] = {
    val contents = scala.io.Source.fromFile(path).getLines.mkString("\n")
    val service = Json.parse(contents).as[Service]
    Lint(All).validate(service)
  }

}
