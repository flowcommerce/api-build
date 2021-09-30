package io.flow.oneapi

import io.apibuilder.spec.v0.models._
import io.flow.build.BuildType

object TypeRewriterDefaults {
  val FieldDescriptions = Map(
    "id" -> "Globally unique identifier",
    "number" -> "Client's unique identifier for this object",
    "organization" -> "Refers to your organization's account identifier"
  )
}

case class TypeRewriter(buildType: BuildType, localTypes: Set[String]) {
  def localizeModel(model: Model): Model = {
    model.copy(
      fields = model.fields.map(localizeField)
    )
  }

  def localizeInterface(interface: Interface): Interface = {
    interface.copy(
      fields = interface.fields.map(localizeField)
    )
  }

  def localizeField(field: Field): Field = {
    field.copy(
      `type` = localizeType(field.`type`),
      description = field.description.orElse {
        TypeRewriterDefaults.FieldDescriptions.get(field.name)
      }
    )
  }

  def localizeUnion(union: Union): Union = {
    union.copy(
      types = union.types.map(localizeUnionType)
    )
  }

  def localizeUnionType(ut: UnionType): UnionType = {
    ut.copy(
      `type` = localizeType(ut.`type`)
    )
  }

  def normalizeName(service: Service, resource: Resource): Resource = {
    val qualifiedName = withNamespace(service, resource.`type`)

    val finalType = if (localTypes.contains(qualifiedName)) {
      parser.toTypeLabel(parser.parse(qualifiedName))
    } else {
      qualifiedName
    }

    resource.copy(
      `type` = finalType
    )
  }

  private[this] def localizeType(name: String): String = {
    buildType match {
      case BuildType.Api | BuildType.ApiEvent => TextDatatypeParser.toTypeLabel(
        TextDatatypeParser.parse(localTypes, name)
      )
      case BuildType.ApiInternal | BuildType.ApiInternalEvent | BuildType.ApiPartner | BuildType.ApiMisc | BuildType.ApiMiscEvent => name
    }
  }

}


