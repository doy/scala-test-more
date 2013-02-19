name         := "scala-test-builder"

version      := "0.01"

scalaVersion := "2.10.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
