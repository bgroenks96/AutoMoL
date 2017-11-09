addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val commonSettings = Seq(
    organization := "edu.osu.cse.groenkeb",
    scalaVersion := "2.11.8",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % Test,
      "junit" % "junit" % "4.11" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test
    )
)

val circeVersion = "0.9.0-M2"

lazy val core = project.enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    name := "automol-core"
  )

lazy val proofEngine = project.in(file("proof-engine")).dependsOn(core)
    .settings(
      commonSettings,
      name := "automol-proof-engine"
    )

lazy val modelvf = project.dependsOn(proofEngine)
    .settings(
      commonSettings,
      name := "automol-modelvf"
    )

lazy val webUtils = project.in(file("web-utils")).dependsOn(core).enablePlugins(ScalaJSPlugin)
    .settings(
      commonSettings,
      name := "automol-web-utils",
      libraryDependencies ++= Seq(
            "io.circe" %%% "circe-core",
            "io.circe" %%% "circe-generic",
            "io.circe" %%% "circe-parser"
        ).map(_ % circeVersion)
    )

lazy val server = project.dependsOn(webUtils, modelvf)
    .settings(
        commonSettings,
        name := "automol-server",
        libraryDependencies ++= Seq(
            "org.http4s" %% "http4s-blaze-server" % "0.18.0-M5",
            "org.http4s" %% "http4s-circe" % "0.18.0-M5",
            "org.http4s" %% "http4s-dsl" % "0.18.0-M5"
        )
    )

// In the future, to avoid forcing the proof engine to be ScalaJS compatible, we should make webx
// only rely on 'core' and send messages to a server to do the proof work for us.
lazy val webx = project.dependsOn(webUtils).enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    name := "automol-webx",
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.3"
    )
  )
