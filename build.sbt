import Common._
import Dependencies._

ThisBuild / crossScalaVersions := List("2.12.21", "2.13.18", "3.3.7")
ThisBuild / organization       := "com.eed3si9n"
ThisBuild / version            := "0.5.0-SNAPSHOT"
ThisBuild / homepage           := Some(url("http://eed3si9n.com/treehugger"))
ThisBuild / licenses           := Seq(
  "MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")
)
ThisBuild / description := "a library to code Scala programmatically."
ThisBuild / scalacOptions += "-deprecation"

val commonSettings = Seq(
  Test / parallelExecution   := false,
  scalafmtOnCompile          := true,
  initialCommands in console :=
    """import treehugger.forest._
      |import definitions._
      |import treehuggerDSL._""".stripMargin
) ++ sonatypeSettings

val library = project
  .in(file("library"))
  .settings(commonSettings)
  .settings(
    name := "treehugger",
    libraryDependencies ++= libDeps(scalaVersion.value)
    // ,sourceDirectory in (Pamflet, pf) := (baseDirectory in ThisBuild).value / "docs"
  )

val bridge = project
  .in(file("bridge"))
  .dependsOn(library)
  .settings(commonSettings)

lazy val root = project
  .in(file("."))
  .aggregate(library, bridge)
  .settings(commonSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    unidocProjectFilter in (ScalaUnidoc, unidoc)           := inAnyProject -- inProjects(bridge),
    unidocConfigurationFilter in (TestScalaUnidoc, unidoc) := inConfigurations(Compile, Test)
  )
  .settings(
    name               := "treehugger",
    publish / skip     := true,
    crossScalaVersions := Nil,
  )
