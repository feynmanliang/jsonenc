import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.feynmanliang",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Shapeless Json Encoder",
    libraryDependencies ++= Seq(
      shapeless % Compile,
      scalatest % Test
    )
  )
