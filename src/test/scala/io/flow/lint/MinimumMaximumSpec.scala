package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class MinimumMaximumSpec extends FunSpec with Matchers {

  val linter = linters.MinimumMaximum

  def buildServiceWithModel(
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    default: Option[String] = None
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "user",
          fields = Seq(
            Services.buildField(
              name = "email",
              default = default,
              minimum = minimum,
              maximum = maximum
            )
          )
        )
      )
    )
  }

  def buildServiceWitCurrencyhModel(
   minimum: Option[Long] = None,
   maximum: Option[Long] = None,
   default: Option[String] = None
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "currency",
          fields = Seq(
            Services.buildField(
              name = "currency",
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

  it("Model w/out min/max is fine") {
    linter.validate(buildServiceWithModel()) should be(Nil)
  }

  it("Model w/ invalid min/max") {
    linter.validate(buildServiceWithModel(minimum = Some(-1), maximum = Some(-1))) should be(
      Seq(
        "Model user Field[email]: Minimum must be >= 0 and not -1",
        "Model user Field[email]: Maximum must be 100 and not -1"
      )
    )
  }

  it("Model w/ default > min") {
    linter.validate(buildServiceWithModel(
      default = Some("0"),
      minimum = Some(1)
    )) should be(
      Seq("Model user Field[email]: Default must be >= minimum[1] and not 0")
    )
  }

  it("Model w/ currency max of 3 is fine") {
    linter.validate(buildServiceWitCurrencyhModel(maximum = Some(3))) should be(Nil)
  }

  it("Model w/ currency max of 5 fails") {
    linter.validate(buildServiceWitCurrencyhModel(maximum = Some(5))) should be(Seq("Model currency Field[currency]: Maximum must be 3 and not 5"))
  }

  it("Model w/ non numeric default is ignored") {
    linter.validate(buildServiceWithModel(
      default = Some("one thousand"),
      minimum = Some(1)
    )) should be(Nil)
  }
  
  it("Param w/out min/max is fine") {
    linter.validate(buildServiceWithParameter()) should be(Nil)
  }

  it("Parameter w/ invalid min/max") {
    linter.validate(buildServiceWithParameter(minimum = Some(-1), maximum = Some(25))) should be(
      Seq(
        "Resource users GET /users Parameter email: Minimum must be >= 0 and not -1",
        "Resource users GET /users Parameter email: Maximum must be 100 and not 25"
      )
    )
  }

  it("Parameter w/ array type requires a max") {
    linter.validate(buildServiceWithParameter(paramType = "[string]")) should be(
      Seq(
        "Resource users GET /users Parameter email: Missing maximum. All parameters that are arrays must have a maximum set to 100"
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
}
