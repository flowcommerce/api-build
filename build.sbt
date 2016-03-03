import com.github.retronym.SbtOneJar._

oneJarSettings

name := "api-lint"

organization := "io.flow"

scalaVersion in ThisBuild := "2.11.7"

version := "0.0.20"

exportJars := true

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.4.6",
      "com.ning" % "async-http-client" % "1.9.33",
      "org.scalatest" %% "scalatest" % "2.2.6" % Test
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
