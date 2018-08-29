import com.github.retronym.SbtOneJar._
import sbt.Credentials

oneJarSettings

name := "api-build"

organization := "io.flow"

scalaVersion in ThisBuild := "2.12.6"

version := "0.1.90"

exportJars := true

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "io.flow" %% "lib-util" % "0.1.0",
      "io.flow" %% "apibuilder-validation" % "0.2.1",
      "com.typesafe.play" %% "play-json" % "2.6.10",
      "com.ning" % "async-http-client" % "1.9.40",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
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
