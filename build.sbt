ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.1"

lazy val root = (project in file("."))
  .settings(
    name := "Park"
  )

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
