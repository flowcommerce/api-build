package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._

object Services {

  val Base = Service(
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

  def buildSimpleResource(
    `type`: String,
    plural: String,
    method: Method,
    path: String,
    parameters: Seq[Parameter] = Nil,
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
    responseType: String,
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

  def buildResponse(
    code: Int = 200,
    `type`: String
  ) = Response(
    code = ResponseCodeInt(code),
    `type` = `type`
  )

}
