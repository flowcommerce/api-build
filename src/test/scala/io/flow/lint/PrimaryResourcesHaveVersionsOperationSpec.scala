package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class PrimaryResourcesHaveVersionsOperationSpec extends FunSpec with Matchers {

  val linter = linters.PrimaryResourcesHaveVersionsOperation

  def buildService(operations: Seq[Operation]): Service = {
    Services.Base.copy(
      resources = Seq(
        Services.buildResource(
          `type` = "organization",
          plural = "organizations",
          operations = operations
        )
      )
    )
  }

  private[this] val get = Services.buildSimpleOperation(
    path = "/organizations",
    responseType = "[organization]"
  )

  private[this] val getNoVersions = Services.buildSimpleOperation(
    path = "/organizations",
    responseType = "[organization]",
    attributes = Seq(Attribute("non-crud", Json.obj()))
  )

  it("valid resource is left alone") {
    linter.validate(
      buildService(
        Seq(
          get,
          Services.buildSimpleOperation(
            path = "/organizations/versions",
            responseType = "[organization_version]"
          )
        )
      )
    ) should be(Nil)
  }

  it("validates if /versions resource is missing") {
    linter.validate(
      buildService(Seq(get))
    ) should be(
      Seq("Resource organizations GET /organizations: Missing versions operation at path /organizations/versions")
    )
  }

  it("validates /versions resource not required when 'non-crud' attribute is present") {
    linter.validate(
      buildService(Seq(getNoVersions))
    ) should be(Nil)
  }

  it("validates version path at root") {
    linter.validate(
      buildService(
        Seq(
          Services.buildSimpleOperation(
            path = "/",
            responseType = "[organization]"
          ),
          Services.buildSimpleOperation(
            path = "/versions",
            responseType = "[organization_version]"
          )
        )
      )
    ) should be(Nil)
  }

  it("validates response type") {
    linter.validate(
      buildService(
        Seq(
          Services.buildSimpleOperation(
            path = "/",
            responseType = "[organization]"
          ),
          Services.buildSimpleOperation(
            path = "/versions",
            responseType = "[organization]"
          )
        )
      )
    ) should be(Seq("Resource organizations GET /versions: 2xx response type should be 'organization_version' and not organization"))
  }

}