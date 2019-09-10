import sbt._
import Keys._

object Dependencies {
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.13." => List("org.specs2" %% "specs2-core" % "4.7.0" % Test)
    case _                         => List("org.specs2" %% "specs2-core" % "3.9.5" % Test)
  }

  def libDeps(sv: String) =
    specs2(sv)
}
