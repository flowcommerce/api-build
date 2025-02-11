package io.flow.build

case class Config(
  buildType: BuildType = BuildType.Api,
  protocol: String = "https",
  domain: String = "api.flow.io",
  buildCommand: String = "all",
  apis: Seq[String] = Seq(),
  output: java.nio.file.Path = java.nio.file.Paths.get("/tmp"),
)
