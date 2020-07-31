package io.flow.lint

import io.apibuilder.spec.v0.models._
import play.api.libs.json.{JsObject, Json}

object Services {

  val Base: Service = Service(
    apidoc = Apidoc(
      version = "0.9.50"
    ),
    name = "api-lint-test",
    organization = Organization(
      key = "flow"
    ),
    application = Application(
      key = "api-lint-test"
    ),
    namespace = "io.flow.api.lint.test",
    version = "0.0.1",
    info = Info()
  )

  def buildEnum(
    name: String,
    plural: String,
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    values: Seq[EnumValue] = Nil,
    attributes: Seq[Attribute] = Nil
  ): Enum = {
    Enum(
      name,
      plural,
      description,
      deprecation,
      values,
      attributes
    )
  }

  def buildUnion(
    name: String,
    discriminator: Option[String] = None,
    types: Seq[UnionType] = Nil
  ): Union = {
    Union(
      name = name,
      plural = name + "s",
      discriminator = discriminator,
      types = types
    )
  }

  def buildUnionType(
    `type`: String
  ): UnionType = {
    UnionType(
      `type` = `type`
    )
  }

  /**
    * Builds an attribute with the name 'linter' containing an ignore hint
    */
  def buildLinterIgnoreAttribute(
    values: Seq[String]
  ): Attribute = {
    buildAttribute(
      name = "linter",
      value = Json.obj(
        "ignore" -> values
      )
    )
  }

  def buildAttribute(
    name: String,
    value: JsObject
  ): Attribute = {
    Attribute(
      name = name,
      value = value
    )
  }

  def buildModel(
    name: String,
    fields: Seq[Field] = Nil
  ): Model = {
    Model(
      name = name,
      plural = name + "s",
      fields = fields
    )
  }

  def buildInterface(
    name: String,
    fields: Seq[Field] = Nil
  ): Interface = {
    Interface(
      name = name,
      plural = name + "s",
      fields = fields
    )
  }

  def buildField(
    name: String,
    `type`: String = "string",
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    default: Option[String] = None,
    required: Boolean = true,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    example: Option[String] = None
  ): Field = {
    Field(
      name = name,
      `type` = `type`,
      description = description,
      deprecation = deprecation,
      default = default,
      required = required,
      minimum = minimum,
      maximum = maximum,
      example = example
    )
  }

  def buildSimpleModel(name: String, fields: Seq[String] = Nil): Model = {
    Services.buildModel(
      name = name,
      fields = fields.map(Services.buildField(_))
    )
  }

  def buildSimpleEnum(name: String, plural: String, values: Seq[EnumValue] = Nil): Enum = {
    Services.buildEnum(
      name = name,
      plural = plural,
      values = values
    )
  }

  def buildSimpleResource(
    `type`: String,
    plural: String,
    method: Method,
    path: String,
    parameters: Seq[Parameter] = Nil,
    attributes: Seq[Attribute] = Nil,
    responseCode: Int,
    responseType: String
  ): Resource = {
    Resource(
      `type` = `type`,
      plural = plural,
      operations = Seq(
        Operation(
          method = method,
          path = path,
          parameters = parameters,
          attributes = attributes,
          responses = Seq(
            buildResponse(responseCode, responseType)
          )
        )
      )
    )
  }

  def buildResource(
    `type`: String,
    plural: String,
    operations: Seq[Operation] = Nil
  ): Resource = Resource(
    `type` = `type`,
    plural = plural,
    operations = operations
  )

  def buildSimpleOperation(
    method: Method = Method.Get,
    path: String,
    parameters: Seq[Parameter] = Nil,
    responseCode: Int = 200,
    responseType: String = "string",
    attributes: Seq[Attribute] = Nil
  ): Operation = {
    Operation(
      method = method,
      path = path,
      parameters = parameters,
      responses = Seq(
        buildResponse(responseCode, responseType)
      ),
      attributes = attributes
    )
  }

  def buildParameter(
    name: String = "q",
    `type`: String = "String",
    maximum: Option[Long] = None,
    example: Option[String] = None
  ) = Parameter(
    name = name,
    `type` = `type`,
    location = ParameterLocation.Query,
    required = false,
    maximum = maximum,
    example = example
  )

  def buildResponse(
    code: Int = 200,
    `type`: String
  ) = Response(
    code = ResponseCodeInt(code),
    `type` = `type`
  )

  def buildServiceByPath(path: String): Service = {
    Services.Base.copy(
      resources = Seq(
        Resource(
          `type` = "organization",
          plural = "organizations",
          operations = Seq(
            Operation(
              method = Method.Get,
              path = path,
              parameters = Nil,
              responses = Seq(Services.buildResponse(`type` = "unit"))
            )
          )
        )
      )
    )
  }
}
