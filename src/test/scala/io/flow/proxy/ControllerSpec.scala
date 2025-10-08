package io.flow.proxy

import io.apibuilder.spec.v0.models.Service
import io.flow.build.{BuildConfig, ServerConfig}
import io.flow.lint.Services
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ControllerSpec extends AnyFunSpec with Matchers {
  private val service: Service = Services.Base

  private val resolveHost: Service => String = {
    val buildConfig = BuildConfig(
      protocol = "https",
      domain = "api.flow.io",
      productionServerConfigs = Seq(ServerConfig(name = service.name, host = "http://lint.me")),
      output = java.nio.file.Paths.get("/tmp"),
    )
    val serviceHostResolver = ServiceHostResolver(Nil)
    Controller.makeProductionHostProvider(buildConfig, serviceHostResolver)
  }

  describe("production host resolver") {
    describe("when server config available") {
      it("should use configured host") {
        resolveHost(service) shouldBe "http://lint.me"
      }
    }

    describe("when server config not available") {
      it("should use constructed host name") {
        resolveHost(service.copy(name = "foo")) shouldBe "https://foo.api.flow.io"
      }
    }
  }
}
