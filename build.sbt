name := "soundmining-tools"

organization := "net.soundmining"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.8"

Test / testOptions := Seq(Tests.Filter(s => s.endsWith("Spec")))

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11"

libraryDependencies += "de.sciss" %% "scalaosc" % "1.3.1" withSources()
