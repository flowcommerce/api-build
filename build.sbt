import com.github.retronym.SbtOneJar._
import sbt.Credentials

oneJarSettings

name := "api-build"

organization := "io.flow"

scalaVersion in ThisBuild := "2.12.10"

version := "0.2.35"

exportJars := true

lazy val root = project
  .in(file("."))
  .settings(
    scalacOptions += "-P:silencer:pathFilters=src/main/scala/io/flow/generated/.*",
    libraryDependencies ++= Seq(
      "io.flow" %% "lib-util" % "0.1.31",
      "io.flow" %% "apibuilder-validation" % "0.4.8",
      "com.typesafe.play" %% "play-json" % "2.7.4",
      "com.ning" % "async-http-client" % "1.9.40",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.2"),
      "com.github.ghik" %% "silencer-lib" % "1.4.2" % Provided
    )
  )

resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/"

credentials += Credentials(
  "Artifactory Realm",
  "flow.jfrog.io",
  System.getenv("ARTIFACTORY_USERNAME"),
  System.getenv("ARTIFACTORY_PASSWORD")
)

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
