package io.flow.build

case class Config(
  buildType: BuildType = BuildType.Api,
  buildCommand: String = "all",
  apis: Seq[String] = Seq(),
)
