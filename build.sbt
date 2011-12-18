organization := "com.eed3si9n"

name := "treehugger"

version := "0.0.1-SNAPSHOT"

libraryDependencies <++= (scalaVersion) { (sv) => sv match {
  case "2.8.1"   => Seq("org.specs2" %% "specs2" % "1.5" % "test",
                        "org.specs2" %% "specs2-scalaz-core" % "5.1-SNAPSHOT" % "test")
  case "2.9.0-1" => Seq("org.specs2" %% "specs2" % "1.6.1" % "test",
                        "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2" % "test")
  case _ =>         Seq("org.specs2" %% "specs2" % "1.6.1" % "test",
                        "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test")
}}

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
                  "releases"  at "http://scala-tools.org/repo-releases")
