import sbt._
import Keys._

object Dependencies {
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.13.0-M2" => Nil
    case x if x startsWith "2.12." => List("org.specs2" %% "specs2-core" % "3.8.9" % Test)
    case x if x startsWith "2.11." => List("org.specs2" %% "specs2" % "2.3.12" % Test)
    case x if x startsWith "2.10." => List("org.specs2" %% "specs2" % "2.1.1" % Test)
    case x if x startsWith "2.9."  => List("org.specs2" %% "specs2" % "1.12.3" % Test)
  }

  def libDeps(sv: String) =
    specs2(sv)
}
