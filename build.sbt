import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
      name := "ham",
      scalacOptions += "-Ypartial-unification",
      libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0",
      libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.0",
      libraryDependencies += "io.monix" %% "minitest" % "2.3.2" % "test",
      testFrameworks += new TestFramework("minitest.runner.Framework")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
