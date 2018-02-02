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
val parsebackVersion = "0.3"

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

lazy val parseExt = project.in(file("parse-ext")).dependsOn(core)
    .settings(
      commonSettings,
      name := "automol-parse-ext",
      resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
      libraryDependencies ++= Seq(
        "com.codecommit" %% "parseback-core" % parsebackVersion,
        "com.codecommit" %% "parseback-cats" % parsebackVersion
      )
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

lazy val server = project.dependsOn(webUtils, modelvf, parseExt)
    .settings(
        commonSettings,
        name := "automol-server",
        libraryDependencies ++= Seq(
            "org.http4s" %% "http4s-blaze-server" % "0.18.0-M5",
            "org.http4s" %% "http4s-circe" % "0.18.0-M5",
            "org.http4s" %% "http4s-dsl" % "0.18.0-M5"
        )
    )

lazy val webx = project.dependsOn(webUtils).enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    name := "automol-webx",
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    jsDependencies += "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js",
    libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.3",
        "be.doeraene" %%% "scalajs-jquery" % "0.9.2"
    )
  )
