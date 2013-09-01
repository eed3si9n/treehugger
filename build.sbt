import Common._
import Dependencies._

val commonSettings = Seq(
    organization := "com.eed3si9n",
    version := "0.3.0",
    scalaVersion := "2.10.2",
    crossScalaVersions := Seq("2.10.2", "2.9.2", "2.9.1"),
    homepage := Some(url("http://eed3si9n.com/treehugger")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
    description := "a library to code Scala programmatically.",
    initialCommands in console := """import treehugger.forest._
                                    |import definitions._
                                    |import treehuggerDSL._""".stripMargin,
    resolvers ++= Seq("sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                      "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases")
  ) ++ sonatypeSettings ++ customLsSettings

val library = project.in(file("library")).
  settings(commonSettings: _*).
  settings(
    name := "treehugger",
    libraryDependencies ++= libDeps(scalaVersion.value)
  )

val bridge = project.in(file("bridge")).
  settings(commonSettings: _*).
  dependsOn(library)

lazy val root = project.in(file(".")).
  settings(commonSettings: _*).
  settings(
    name := "treehugger",
    publishArtifact := false
  ).
  aggregate(library, bridge)
