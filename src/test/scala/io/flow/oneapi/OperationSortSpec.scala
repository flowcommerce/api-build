package io.flow.oneapi

import io.apibuilder.spec.v0.models.Method
import io.flow.lint.Services
import org.scalatest.{FunSpec, Matchers}

class OperationSortSpec extends FunSpec with Matchers {

  it("static paths") {
    Seq(
      Services.buildSimpleOperation(
        path = "/organizations/id"
      ),
      Services.buildSimpleOperation(
        path = "/organizations"
      )
    ).sortBy(OperationSort.key).map(_.path) should equal(
      Seq("/organizations", "/organizations/id")
    )
  }

  it("GET before POST") {
    Seq(
      Services.buildSimpleOperation(
        method = Method.Post,
        path = "/organizations"
      ),
      Services.buildSimpleOperation(
        method = Method.Get,
        path = "/organizations"
      )
    ).sortBy(OperationSort.key).map(_.method) should equal(
      Seq(Method.Get, Method.Post)
    )
  }

  it("simple dynamic paths") {
    Seq(
      Services.buildSimpleOperation(
        path = "/:organization/experiences/:key"
      ),
      Services.buildSimpleOperation(
        path = "/:organization/experiences"
      )
    ).sortBy(OperationSort.key).map(_.path) should equal(
      Seq("/:organization/experiences", "/:organization/experiences/:key")
    )
  }

  it("complex dynamic paths") {
    Seq(
      Services.buildSimpleOperation(
        path = "/:organization/experiences/:key"
      ),
      Services.buildSimpleOperation(
        path = "/:organization/experiences/items"
      )
    ).sortBy(OperationSort.key).map(_.path) should equal(
      Seq("/:organization/experiences/items", "/:organization/experiences/:key")
    )
  }

}
