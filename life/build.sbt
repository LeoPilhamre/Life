val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Life",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )
