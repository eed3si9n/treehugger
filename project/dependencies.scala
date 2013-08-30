import sbt._
import Keys._

object Dependencies {
  val specs2Version = "2.1.1"
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.9." => "org.specs2" %% "specs2" % "1.12.3"
    case _ => "org.specs2" %% "specs2" % specs2Version
  }

  def libDeps(sv: String) = Seq(
    specs2(sv) % "test"
  )
}
