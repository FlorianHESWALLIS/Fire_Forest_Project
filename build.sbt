ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "Fire_Forest_Project",
    version := "0.1.0",
    Compile / run / fork := true,
    libraryDependencies += "org.scalafx" %% "scalafx" % "21.0.0-R31",
    javaOptions ++= Seq(
      "--module-path", "C:/chemin/vers/javafx-sdk-21.0.3/lib",
      "--add-modules", "javafx.controls,javafx.fxml"
    )
  )
