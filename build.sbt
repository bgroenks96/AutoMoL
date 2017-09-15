lazy val root = (project in file("."))
  .settings(
    name         := "AutoMoL",
    organization := "edu.osu.cse.groenkeb",
    scalaVersion := "2.11.8",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "21.0",
      "org.scalatest" %% "scalatest" % "2.2.2" % Test,
      "junit" % "junit" % "4.11" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test
    ),
  )
