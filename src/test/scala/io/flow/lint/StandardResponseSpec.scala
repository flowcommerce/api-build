package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}

class StandardResponseSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.StandardResponse

  def buildService(
    path: String,
    method: Method,
    responses: Seq[Response]
  ): Service = {
    Services.Base.copy(
      resources = Seq(
        Resource(
          `type` = "organization",
          plural = "organizations",
          operations = Seq(
            Operation(
              method = method,
              path = path,
              parameters = Nil,
              responses = responses
            )
          )
        )
      )
    )
  }

  it("standard codes must not have descriptions") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Get,
        Seq(
          Response(ResponseCodeInt(200), "[organization]", description = Some("Foo")),
          Response(ResponseCodeInt(401), "unit", description = Some("Bar"))
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations/:id Response 200: Must not have a description as this is a globally standard response",
        "Resource organizations GET /organizations/:id Response 401: Must not have a description as this is a globally standard response"
      )
    )
  }

  it("Delete to subresource allows http 200") {
    linter.validate(
      buildService(
        "/organizations/:id/items",
        Method.Delete,
        Seq(
          Response(ResponseCodeInt(200), "[organization]"),
          Response(ResponseCodeInt(401), "unit"),
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(Nil)
  }

  it("204, 401 must have unit type") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Delete,
        Seq(
          Response(ResponseCodeInt(204), "string"),
          Response(ResponseCodeInt(401), "string"),
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations DELETE /organizations/:id Response 204: response must be of type unit and not string",
        "Resource organizations DELETE /organizations/:id Response 401: response must be of type unit and not string"
      )
    )
  }

  it("Get must have 200, 401") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Get,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations/:id: Missing response codes: 200, 401"
      )
    )
  }

  it("Patch must have 401, 404, 422") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Patch,
        Seq(
          Response(ResponseCodeInt(409), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations PATCH /organizations/:id: Missing response codes: 401, 404, 422"
      )
    )
  }

  it("Post must have 401, 422") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Post,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations POST /organizations/:id: Missing response codes: 401, 422"
      )
    )
  }

  it("Put must have 401, 422") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Put,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations PUT /organizations/:id: Missing response codes: 401, 422"
      )
    )
  }

  it("Delete must have 401, 404") {
    linter.validate(
      buildService(
        "/organizations/:id",
        Method.Delete,
        Seq(
          Response(ResponseCodeInt(409), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations DELETE /organizations/:id: Missing response codes: 401, 404"
      )
    )
  }

  it("RequiredResponseCodes has all methods") {
    Method.all.foreach { m =>
      linters.StandardResponse.RequiredResponseCodes.get(m) match {
        case None => fail(s"Method[$m] missing from StandardResponse.RequiredResponseCodes")
        case Some(codes) => {
          codes.sorted == codes match {
            case true => {}
            case false => fail(s"StandardResponse.RequiredResponseCodes[$m] codes must be sorted")
          }
        }
      }
    }
  }

  it("Handles invalid error types") {
    Seq("ordererrors", "error", "[generic_error]").foreach { typ =>
      linter.validate(
        buildService(
        "/organizations/:id",
          Method.Post,
          Seq(
            Response(ResponseCodeInt(200), "[organization]"),
            Response(ResponseCodeInt(401), "unit"),
            Response(ResponseCodeInt(422), typ)
          )
        )
      ) should be(
        Seq(
          s"Resource organizations POST /organizations/:id Response 422: response must be of type *_error and not $typ"
        )
      )
    }
  }

  it("Handles valid error types") {
    Seq(
      "order_error",
      "generic_error",
      "card_error",
      "io.flow.error.v0.models.validation_error",
      "io.flow.error.v0.models.order_error",
      "io.flow.error.v0.unions.order_error"
    ).foreach { typ =>
      linter.validate(
        buildService(
          "/organizations/:id",
          Method.Post,
          Seq(
            Response(ResponseCodeInt(200), "[organization]"),
            Response(ResponseCodeInt(401), "unit"),
            Response(ResponseCodeInt(422), typ)
          )
        )
      ) should be(Nil)
    }
  }

  it("Missing 422 on POST is validated when 'response_codes' attribute is listed as ignored") {
    linter.validate(
      Services.Base.copy(
        resources = Seq(
          Resource(
            `type` = "organization",
            plural = "organizations",
            operations = Seq(
              Operation(
                method = Method.Post,
                path = "/organizations/:id",
                parameters = Nil,
                responses = Seq(
                  Response(ResponseCodeInt(200), "[organization]"),
                  Response(ResponseCodeInt(401), "unit")
                ),
                attributes = Seq(
                  Attribute(
                    name = "linter",
                    value = Json.parse("""{ "ignore": ["response_codes"] }""").as[JsObject]
                  )
                )
              )
            )
          )
        )
      )
    ) should be(Nil)
  }
  
}
