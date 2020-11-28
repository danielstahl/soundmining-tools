name := "soundmining-tools"

organization := "net.soundmining"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.3"

testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Spec")))

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
