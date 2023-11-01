package io.flow.lint

import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DuplicateMethodAndPathSpec extends AnyFunSpec with Matchers {

  private[this] val linter = linters.DuplicateMethodAndPath

  def buildService(
    methodsAndPaths: (Method, String)*
  ): Service = {
    Services.Base.copy(
      models = Seq(
        Services.buildModel("user")
      ),
      resources = Seq(
        Services.buildResource(
          "user",
          operations = methodsAndPaths.map { case (method, path) =>
            Services.buildSimpleOperation(
              method = method,
              path = path,
              responseType = "user"
            )
          }
        )
      )
    )
  }

  it("different paths") {
    linter.validate(
      buildService(
        (Method.Get, "/users"),
        (Method.Get, "/experiences")
      )
    )
  }

  it("different methods") {
    linter.validate(
      buildService(
        (Method.Get, "/users"),
        (Method.Post, "/users")
      )
    ) should be(Nil)
  }

  it("validates duplicate method and path") {
    linter.validate(
      buildService(
        (Method.Get, "/users"),
        (Method.Get, "/users")
      )
    ) should be(
      Seq("1 or more operation paths is duplicated: GET /users")
    )
  }

}
