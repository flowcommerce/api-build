package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class GetSpec extends FunSpec with Matchers {

  val organization = Services.withHealthcheck(
    Services.Base.copy(
      resources = Seq(
        Services.buildSimpleResource(
          `type` = "organization",
          plural = "organizations",
          method = Method.Get,
          path = "/organizations",
          responseCode = 200,
          responseType = "[organization]"
        )
      )
    )
  )

  it("requires at least one GET operation for each resource") {
    Lint(
      Services.withHealthcheck(
        Services.Base.copy(
          resources = Seq(
            Resource(
              `type` = "organization",
              plural = "organizations",
              operations = Nil
            )
          )
        )
      )
    ).validate should be(
      Seq("organizations: Must have at least one operation")
    )
  }

}
