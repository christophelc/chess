import Dependencies._

scalaVersion := "2.13.3"

lazy val chess = (project in file(".")).enablePlugins(JmhPlugin)

libraryDependencies += akka
libraryDependencies ++= logger
libraryDependencies ++= testLib