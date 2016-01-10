package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class PrimaryResourcesHaveVersionsOperationSpec extends FunSpec with Matchers {

  val linter = Lint(Seq(linters.PrimaryResourcesHaveVersionsOperation))

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

}
