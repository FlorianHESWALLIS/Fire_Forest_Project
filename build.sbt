ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "Fire_Forest_Project",
    Compile / run / fork := true,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
