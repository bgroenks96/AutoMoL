lazy val root = (project in file("."))
  .settings(
    name         := "AutoMoL",
    organization := "edu.osu.cse.groenkeb",
    scalaVersion := "2.11.8",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "21.0",
      "junit" % "junit" % "4.0"
    )
  )
