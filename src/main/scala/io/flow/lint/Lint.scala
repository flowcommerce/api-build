package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import play.api.libs.json._

case class Lint(
  linters: Seq[Linter] = Lint.All
) {
  
  def validate(service: Service): Seq[String] = {
    linters.flatMap(_.validate(service))
  }

}

object Lint {

  val All = Seq(
    linters.BadNames,
    linters.CommonFieldTypes,
    linters.CommonParameterTypes,
    linters.CommonParametersHaveNoDescriptions,
    linters.ErrorModels,
    linters.EventModels,
    linters.ExpandableUnionsAreConsistent,
    linters.Get,
    linters.GetByIdIsExpandable,
    linters.LowerCasePaths,
    linters.MappingModels,
    linters.MinimumMaximum,
    linters.ModelsWithOrganizationField,
    linters.PrimaryResourcesHaveVersionsOperation,
    linters.ProxyQueryParameters,
    linters.PublishedEventModels,
    linters.SortAttribute,
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
