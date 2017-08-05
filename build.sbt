import sbt._

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.11.5"
libraryDependencies += "org.scala-graph" %% "graph-dot" % "1.11.5"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0"

val circeVersion = "0.8.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

name := ""
lazy val commonSettings = Seq(
  scalaVersion := "2.12.1",
  organization := "lambda.traceur",
  version := "1.0"
)

lazy val punter = (project in file(".")).
  settings(commonSettings: _*).
  settings( 
    name := "punter", 
    assemblyJarName in assembly := "../../punter.jar",
    mainClass in assembly := Some("Application") 
  )


// To learn more about multi-project builds, head over to the official sbt
// documentation at http://www.scala-sbt.org/documentation.html

