import com.github.retronym.SbtOneJar._

oneJarSettings

name := "api-build"

organization := "io.flow"

scalaVersion in ThisBuild := "2.12.5"

version := "0.1.68"

exportJars := true

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.6.9",
      "com.ning" % "async-http-client" % "1.9.40",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

publishTo := {
  val host = "https://flow.artifactoryonline.com/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
