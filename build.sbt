import Dependencies._

scalaVersion := "2.12.11"

lazy val chess = (project in file(".")).enablePlugins(JmhPlugin)

libraryDependencies += akka
libraryDependencies ++= logger
libraryDependencies ++= testLib

