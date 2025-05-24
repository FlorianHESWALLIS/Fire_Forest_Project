ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaFXApp",
    version := "0.1.0",
    Compile / run / fork := true,
    libraryDependencies += "org.scalafx" % "scalafx_3" % "21.0.0-R31"
  )
