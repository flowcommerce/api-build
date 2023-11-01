package io.flow.lint

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import io.flow.build.BuildType
import play.api.libs.json.Json

case class Lint(
  buildType: BuildType
) {

  def validate(service: Service): Seq[String] = {
    Lint.forBuildType(buildType).flatMap(_.validate(service))
  }

}

object Lint {

  def forBuildType(buildType: BuildType): Seq[Linter] = {
    buildType match {
      case BuildType.ApiMisc | BuildType.ApiMiscEvent =>
        Seq(
          linters.BeaconEventsMustHaveAttributes
        )

      case _ => {
        Seq(
          linters.AllAttributesAreWellKnown,
          linters.BadNames,
          linters.CommonFieldTypes,
          linters.CommonParameterTypes,
          linters.CommonParametersHaveNoDescriptions,
          linters.DuplicateMethodAndPath,
          linters.ErrorModelsV1,
          linters.ErrorModelsV2,
          linters.EventStructure,
          linters.EventUpsertedModels,
          linters.ExpandableUnionsAreConsistent,
          linters.Get,
          linters.GetByIdIsExpandable,
          linters.InclusiveTerminologyLinter,
          linters.LowerCasePaths,
          linters.MappingModels,
          linters.MinimumMaximum,
          linters.ModelsWithOrganizationField,
          linters.PathsDoNotHaveTrailingSlash,
          linters.ProxyQueryParameters,
          linters.PublishedEventModels,
          linters.SortAttribute,
          linters.SortParameterDefault,
          linters.StandardResponse,
          linters.UnionTypesHaveCommonDiscriminator,
          linters.UpsertedDeletedEventModels,
          linters.VersionModels
        )
      }
    }
  }

  def fromFile(buildType: BuildType, path: String): Seq[String] = {
    val source = scala.io.Source.fromFile(path)
    try {
      val contents = source.getLines().mkString("\n")
      val service = Json.parse(contents).as[Service]
      Lint(buildType).validate(service)
    } finally {
      source.close()
    }
  }

}
