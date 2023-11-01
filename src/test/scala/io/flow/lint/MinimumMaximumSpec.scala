package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MinimumMaximumSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.MinimumMaximum

  def buildServiceWithModel(
    modelName: String = "user",
    fieldName: String = "email",
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    default: Option[String] = None
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          modelName,
          fields = Seq(
            Services.buildField(
              name = fieldName,
              default = default,
              minimum = minimum,
              maximum = maximum
            )
          )
        )
      )
    )
  }

  def buildServiceWithParameter(
    paramName: String = "email",
    paramType: String = "string",
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildSimpleModel("user")
      ),
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "user",
          plural = "users",
          method = Method.Get,
          path = "/users",
          responseCode = 200,
          responseType = "[organization]",
          parameters = Seq(
            Parameter(
              name = paramName,
              `type` = paramType,
              location = ParameterLocation.Query,
              required = false,
              minimum = minimum,
              maximum = maximum
            )
          )
        )
      )
    )
  }

  def buildServiceWithParameterAsEnum(
    paramName: String = "test_enum",
    paramType: String = "test_enum_value",
    operationParamType: String = "test_enum",
    minimum: Option[Long] = None,
    maximum: Option[Long] = None
  ): Service = {
    Services.Base.copy(
      enums = Seq(
        Services.buildSimpleEnum(paramName, Seq(EnumValue(name = paramType)))
      ),
      models = Seq(
        Services.buildSimpleModel("user")
      ),
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "user",
          plural = "users",
          method = Method.Get,
          path = "/users",
          responseCode = 200,
          responseType = "[organization]",
          parameters = Seq(
            Parameter(
              name = paramName,
              `type` = s"[$operationParamType]",
              location = ParameterLocation.Query,
              required = false,
              minimum = minimum,
              maximum = maximum
            )
          )
        )
      )
    )
  }

  it("Model w/out min/max is fine") {
    linter.validate(buildServiceWithModel()) should be(Nil)
  }

  it("Model w/ maximum larger than minimum") {
    linter.validate(buildServiceWithModel(minimum = Some(2), maximum = Some(-2))) should be(
      Seq(
        "Model user Field[email]: Maximum, if specified with minimum, must be >= 2 and not -2"
      )
    )
  }

  it("Model w/ default > min") {
    linter.validate(
      buildServiceWithModel(
        default = Some("0"),
        minimum = Some(1)
      )
    ) should be(
      Seq("Model user Field[email]: Default must be >= minimum[1] and not 0")
    )
  }

  it("Model w/ currency max of 3 is fine") {
    linter.validate(buildServiceWithModel(modelName = "currency", fieldName = "currency", maximum = Some(3))) should be(
      Nil
    )
  }

  it("Model w/ currency max of 5 fails") {
    linter.validate(buildServiceWithModel(modelName = "currency", fieldName = "currency", maximum = Some(5))) should be(
      Seq("Model currency Field[currency]: Maximum must be 3 and not 5")
    )
  }

  it("Model w/ country max of 3 is fine") {
    linter.validate(buildServiceWithModel(modelName = "country", fieldName = "country", maximum = Some(3))) should be(
      Nil
    )
  }

  it("Model w/ origin max of 3 is fine") {
    linter.validate(buildServiceWithModel(modelName = "country", fieldName = "origin", maximum = Some(3))) should be(
      Nil
    )
  }

  it("Model w/ country max of 5 fails") {
    linter.validate(buildServiceWithModel(modelName = "country", fieldName = "country", maximum = Some(5))) should be(
      Seq("Model country Field[country]: Maximum must be 3 and not 5")
    )
  }

  it("Model w/ country and fielding name containing country with max of 3 is fine") {
    linter.validate(
      buildServiceWithModel(modelName = "country", fieldName = "some_country_field", maximum = Some(3))
    ) should be(Nil)
  }

  it("Model w/ non numeric default is ignored") {
    linter.validate(
      buildServiceWithModel(
        default = Some("one thousand"),
        minimum = Some(1)
      )
    ) should be(Nil)
  }

  it("Param w/out min/max is fine") {
    linter.validate(buildServiceWithParameter()) should be(Nil)
  }

  it("Parameter w/ invalid min/max") {
    linter.validate(buildServiceWithParameter(minimum = Some(-1), maximum = Some(25))) should be(
      Seq(
        "Resource users GET /users Parameter email: Minimum must be >= 0 and not -1"
      )
    )

    linter.validate(
      buildServiceWithParameter(minimum = Some(-1), maximum = Some(25), paramType = "[string]")
    ) should be(
      Seq(
        "Resource users GET /users Parameter email: Minimum must be >= 0 and not -1",
        "Resource users GET /users Parameter email: Maximum must be 100 and not 25"
      )
    )
  }

  it("Parameter w/ array type requires a max") {
    linter.validate(buildServiceWithParameter(paramType = "[string]")) should be(
      Seq(
        "Resource users GET /users Parameter email: Missing maximum"
      )
    )
  }

  it("Parameter expand is excepted from a max") {
    linter.validate(buildServiceWithParameter(paramName = "expand", maximum = Some(2))) should be(Nil)
  }

  it("Parameter expand has min validated") {
    linter.validate(buildServiceWithParameter(paramName = "expand", minimum = Some(-1))) should be(
      Seq(
        "Resource users GET /users Parameter expand: Minimum must be >= 0 and not -1"
      )
    )
  }

  it("Parameter enum has max validated") {
    linter.validate(buildServiceWithParameterAsEnum(maximum = Some(5))) should be(
      Seq(
        "Resource users GET /users Parameter test_enum: Maximum must be 1 and not 5"
      )
    )
  }

  it("Imported enum has max ignored") {
    linter.validate(
      buildServiceWithParameterAsEnum(operationParamType = "io.flow.common.v0.enums.test", maximum = Some(5))
    ) should be(Nil)
  }
}
