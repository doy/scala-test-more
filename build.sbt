name         := "scala-test-more"

organization := "com.iinteractive"

version      := "0.01"

description  := "Test library based on Perl's Test::More"

homepage     := Some(url("https://github.com/doy/scala-test-more"))

licenses     := Seq(
  "MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")
)

scalaVersion := "2.10.0"

libraryDependencies += "org.scala-tools.testing" % "test-interface" % "0.5"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

testFrameworks += new TestFramework("com.iinteractive.test.sbt.Framework")

publishMavenStyle := true

publishArtifact in Test := false

publishTo := Some(
  "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
)

pomIncludeRepository := { _ => false }

pomExtra := (
  <scm>
    <url>git@github.com:doy/scala-test-more.git</url>
    <connection>scm:git:git@github.com:doy/scala-test-more.git</connection>
  </scm>
  <developers>
    <developer>
      <id>doy</id>
      <name>Jesse Luehrs</name>
      <url>http://tozt.net/</url>
    </developer>
  </developers>
)
