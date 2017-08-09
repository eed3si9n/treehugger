import Common._
import Dependencies._
import UnidocKeys._

val commonSettings = Seq(
    organization := "com.eed3si9n",
    version := "0.4.4-SNAPSHOT",
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.12.3", "2.13.0-M2", "2.11.11", "2.10.6"),
    homepage := Some(url("http://eed3si9n.com/treehugger")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
    description := "a library to code Scala programmatically.",
    scalacOptions += "-deprecation",
    parallelExecution in Test := false,
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
    //,sourceDirectory in (Pamflet, pf) := (baseDirectory in ThisBuild).value / "docs"
  )

val bridge = project.in(file("bridge")).
  settings(commonSettings: _*).
  dependsOn(library)

lazy val root = project.in(file(".")).
  settings(commonSettings: _*).
  settings(unidocSettings: _*).
  settings(
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(bridge),
    unidocConfigurationFilter in (TestScalaUnidoc, unidoc) := inConfigurations(Compile, Test)
  ).
  settings(
    name := "treehugger",
    publishArtifact := false,
    publish := (),
    publishLocal := ()
    //,pf := (pf in library).value
  ).
  aggregate(library, bridge)
