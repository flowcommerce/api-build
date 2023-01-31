package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}

class BadNamesSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.BadNames

  def buildModel(
    fieldName: String,
    attributes: Seq[Attribute] = Nil
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel(
          "user",
          Seq(
            Services.buildField(
              name = fieldName
            )
          ),
          attributes
        )
      )
    )
  }

  def buildModelIgnored(
     fieldName: String
  ): Service = {
    buildModel(fieldName, Seq(Attribute("linter", value = Json.parse("""{ "ignore": ["bad_names"] }""").as[JsObject])))
  }

  def buildResource(
    paramName: String,
    attributes: Seq[Attribute] = Nil
  ): Service = {
    buildModel("id").copy(
      resources = Seq(
        Services.buildResource(
          `type` = "user",
          operations = Seq(
            Services.buildSimpleOperation(
              path = "/users",
              responseType = "[user]",
              parameters = Seq(
                Services.buildParameter(paramName)
              ),
              attributes = attributes
            )
          )
        )
      )
    )
  }

  def buildResourceIgnored(
    paramName: String
  ): Service = {
    buildResource(paramName, Seq(Attribute("linter", value = Json.parse("""{ "ignore": ["bad_names"] }""").as[JsObject])))
  }

  it("model fields") {
    linter.validate(buildModel("ip_address")) should be(
      Seq("Model user Field[ip_address]: Name must be 'ip'")
    )

    linter.validate(buildModel("postal_code")) should be(
      Seq("Model user Field[postal_code]: Name must be 'postal'")
    )
  }

  it("model fields ignored") {
    linter.validate(buildModelIgnored("ip_address")) should be(
      Nil
    )

    linter.validate(buildModelIgnored("postal_code")) should be(
      Nil
    )
  }


  it("resource parameter names") {
    linter.validate(buildResource("ip_address")) should be(
      Seq("Resource users GET /users Parameter ip_address: Name must be 'ip'")
    )

    linter.validate(buildResource("postal_code")) should be(
      Seq("Resource users GET /users Parameter postal_code: Name must be 'postal'")
    )
  }

  it("resource parameter names ignored") {
    linter.validate(buildResourceIgnored("ip_address")) should be(
      Nil
    )

    linter.validate(buildResourceIgnored("postal_code")) should be(
      Nil
    )
  }

}
