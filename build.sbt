name := "soundmining-tools"

organization := "net.soundmining"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.5"

testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Spec")))

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.0.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"
