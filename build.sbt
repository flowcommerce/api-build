name := "api-build"

organization := "io.flow"

ThisBuild / scalaVersion := "2.13.6"

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Ypatmat-exhaust-depth", "100", // Fixes: Exhaustivity analysis reached max recursion depth, not all missing cases are reported.
  "-Wconf:src=generated/.*:silent",
  "-Wconf:src=target/.*:silent", // silence the unused imports errors generated by the Play Routes
)

assembly / assemblyMergeStrategy := {
  case PathList("io", "flow", _*) =>
    // we have multiple copies of apibuilder generated code
    // just take the first one, it's no worse than whatever happens in production
    MergeStrategy.first
  case "module-info.class" =>
    MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

lazy val root = project
  .in(file("."))
  .settings(
    scalacOptions ++= allScalacOptions,
    libraryDependencies ++= Seq(
      "io.flow" %% "lib-util" % "0.2.0",
      "io.apibuilder" %% "apibuilder-validation" % "0.4.33",
      "com.typesafe.play" %% "play-json" % "2.9.2",
      "com.ning" % "async-http-client" % "1.9.40",
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
      "com.github.scopt" %% "scopt" % "4.0.1",
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
version := "0.3.3"

