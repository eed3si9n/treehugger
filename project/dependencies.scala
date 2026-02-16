import sbt._
import Keys._

object Dependencies {
  def specs2(sv: String) = "org.specs2" %% "specs2-core" % "4.23.0" % Test

  def libDeps(sv: String) = List(
    specs2(sv)
  )
}
