import sbt._
import Keys._

object Dependencies {
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.11." => "org.specs2" %% "specs2" % "2.3.12"
    case x if x startsWith "2.10." => "org.specs2" %% "specs2" % "2.1.1"
    case x if x startsWith "2.9."  => "org.specs2" %% "specs2" % "1.12.3"
  }

  def libDeps(sv: String) = Seq(
    specs2(sv) % "test"
  )
}
