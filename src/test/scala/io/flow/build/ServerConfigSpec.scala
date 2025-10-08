package io.flow.build

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ServerConfigSpec extends AnyFunSpec with Matchers {
  it("parses server list") {
    val yaml =
      """
        |# comments
        |
        |servers:
        |  - name: catalog
        |    host: http://localhost:7121
        |  - name: billing
        |    host: http://localhost:6071
        |""".stripMargin
    ServerConfig.parseYaml(yaml) shouldBe Right(
      Seq(
        ServerConfig("catalog", "http://localhost:7121"),
        ServerConfig("billing", "http://localhost:6071"),
      ),
    )
  }

  it("fails to parse") {
    ServerConfig.parseYaml("not yaml") shouldBe Left("DecodingFailure at .servers: Missing required field")
  }
}
