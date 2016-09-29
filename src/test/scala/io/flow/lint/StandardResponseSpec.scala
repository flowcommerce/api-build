package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class StandardResponseSpec extends FunSpec with Matchers {

  val linter = linters.StandardResponse

  def buildService(
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
              path = "/organizations",
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
        Method.Get,
        Seq(
          Response(ResponseCodeInt(200), "[organization]", description = Some("Foo")),
          Response(ResponseCodeInt(401), "unit", description = Some("Bar"))
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations Response 200: Must not have a description as this is a globally standard response",
        "Resource organizations GET /organizations Response 401: Must not have a description as this is a globally standard response"
      )
    )
  }

  it("204, 401 must have unit type") {
    linter.validate(
      buildService(
        Method.Delete,
        Seq(
          Response(ResponseCodeInt(204), "string"),
          Response(ResponseCodeInt(401), "string"),
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations DELETE /organizations Response 204: response must be of type unit and not string",
        "Resource organizations DELETE /organizations Response 401: response must be of type unit and not string"
      )
    )
  }

  it("Get must have 200, 401") {
    linter.validate(
      buildService(
        Method.Get,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations GET /organizations: Missing response codes: 200, 401"
      )
    )
  }

  it("Patch must have 401, 404, 422") {
    linter.validate(
      buildService(
        Method.Patch,
        Seq(
          Response(ResponseCodeInt(409), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations PATCH /organizations: Missing response codes: 401, 404, 422"
      )
    )
  }

  it("Post must have 401, 422") {
    linter.validate(
      buildService(
        Method.Post,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations POST /organizations: Missing response codes: 401, 422"
      )
    )
  }

  it("Put must have 401, 422") {
    linter.validate(
      buildService(
        Method.Put,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations PUT /organizations: Missing response codes: 401, 422"
      )
    )
  }

  it("Delete must have 204, 401, 404") {
    linter.validate(
      buildService(
        Method.Delete,
        Seq(
          Response(ResponseCodeInt(409), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations DELETE /organizations: Missing response codes: 204, 401, 404"
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
    Seq("error", "[error]").foreach { typ =>
      linter.validate(
        buildService(
          Method.Post,
          Seq(
            Response(ResponseCodeInt(200), "[organization]"),
            Response(ResponseCodeInt(401), "unit"),
            Response(ResponseCodeInt(422), typ)
          )
        )
      ) should be(
        Seq(
          s"Resource organizations POST /organizations Response 422: response must be of type io.flow.error.v0.* and not $typ"
        )
      )
    }
  }

  it("Handles valid error types") {
    Seq("io.flow.error.v0.models.validation_error", "io.flow.error.v0.models.order_error", "io.flow.error.v0.unions.order_error").foreach { typ =>
      linter.validate(
        buildService(
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
  
}
