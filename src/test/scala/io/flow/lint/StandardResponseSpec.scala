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

  it("Patch must have 200, 401, 404, 422") {
    linter.validate(
      buildService(
        Method.Patch,
        Seq(
          Response(ResponseCodeInt(409), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations PATCH /organizations: Missing response codes: 200, 401, 404, 422"
      )
    )
  }

  it("Post must have 201, 401, 422") {
    linter.validate(
      buildService(
        Method.Post,
        Seq(
          Response(ResponseCodeInt(404), "unit")
        )
      )
    ) should be(
      Seq(
        "Resource organizations POST /organizations: Missing response codes: 201, 401, 422"
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

}
