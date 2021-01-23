

import sbt._


object Dependencies {

  val AkkaVersion = "2.6.11"
  val akka = "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion

  val logger = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2")

  val cats = "org.typelevel" %% "cats-core" % "2.1.1"

  val testLib = Seq("org.specs2" %% "specs2-core" % "4.10.0" % "test")
}
