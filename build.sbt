name := "api-build"

organization := "io.flow"

ThisBuild / scalaVersion := "2.13.13"
ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")
enablePlugins(GitVersioning)
git.useGitDescribe := true
coverageExcludedFiles := ".*\\/src/main/scala/io/flow/generated\\/.*"
coverageDataDir := file("target/scala-2.13")
coverageHighlighting := true
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 48
coverageMinimumBranchTotal := 52

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Ypatmat-exhaust-depth",
  "100", // Fixes: Exhaustivity analysis reached max recursion depth, not all missing cases are reported.
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
    scalafmtOnCompile := true,
    scalacOptions ++= allScalacOptions ++ Seq("-release", "17"),
    libraryDependencies ++= Seq(
      "io.flow" %% "lib-util" % "0.2.43",
      "io.apibuilder" %% "apibuilder-validation" % "0.4.33",
      "com.typesafe.play" %% "play-json" % "2.9.4",
      "com.ning" % "async-http-client" % "1.9.40",
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-effect" % "2.3.3",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "com.github.scopt" %% "scopt" % "4.1.0",
    ),
  )

resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/"
Test / javaOptions ++= Seq(
  "--add-exports=java.base/sun.security.x509=ALL-UNNAMED",
  "--add-opens=java.base/sun.security.ssl=ALL-UNNAMED",
)
credentials += Credentials(
  "Artifactory Realm",
  "flow.jfrog.io",
  System.getenv("ARTIFACTORY_USERNAME"),
  System.getenv("ARTIFACTORY_PASSWORD"),
)

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
version := "0.3.24"
