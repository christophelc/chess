import Dependencies._

scalaVersion := "2.13.3"

lazy val chess = (project in file(".")).enablePlugins(JmhPlugin)

scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)

libraryDependencies += akka
libraryDependencies ++= logger
libraryDependencies ++= testLib
