ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "Tower-Defence"
  )
