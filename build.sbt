name         := "scala-test-builder"

version      := "0.01"

scalaVersion := "2.10.0"

libraryDependencies += "org.scala-tools.testing" % "test-interface" % "0.5"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

testFrameworks += new TestFramework("org.perl8.test.sbt.Framework")
