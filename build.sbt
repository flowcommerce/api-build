name := "api-build"

organization := "io.flow"

scalaVersion in ThisBuild := "2.12.10"

version := "0.2.53"

assemblyMergeStrategy in assembly := {
  case PathList("io", "flow", _*) =>
    // we have multiple copies of apibuilder generated code
    // just take the first one, it's no worse than whatever happens in production
    MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lazy val root = project
  .in(file("."))
  .settings(
    scalacOptions += "-P:silencer:pathFilters=src/main/scala/io/flow/generated/.*",
    libraryDependencies ++= Seq(
      "io.flow" %% "lib-util" % "0.1.34",
      "io.apibuilder" %% "apibuilder-validation" % "0.4.17",
      "com.typesafe.play" %% "play-json" % "2.7.4",
      "com.ning" % "async-http-client" % "1.9.40",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "com.github.scopt" %% "scopt" % "3.7.1",
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
