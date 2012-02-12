organization := "com.eed3si9n"

name := "treehugger"

version := "0.0.1-SNAPSHOT"

initialCommands in console := """import treehugger.forest._
                                |import definitions._
                                |import treehuggerDSL._""".stripMargin

libraryDependencies <++= (scalaVersion) { (sv) => sv match {
  case "2.8.1"   => Seq("org.specs2" %% "specs2" % "1.5" % "test",
                        "org.specs2" %% "specs2-scalaz-core" % "5.1-SNAPSHOT" % "test")
  case "2.9.0-1" => Seq("org.specs2" %% "specs2" % "1.6.1" % "test",
                        "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2" % "test")
  case _ =>         Seq("org.specs2" %% "specs2" % "1.7.1" % "test",
                        "org.pegdown" % "pegdown" % "1.0.2")
}}

parallelExecution in Test := false

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
                  "releases"  at "http://scala-tools.org/repo-releases")

publishArtifact in (Compile, packageBin) := true

publishArtifact in (Test, packageBin) := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

publishTo <<= version { (v: String) =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if(v endsWith "-SNAPSHOT") Some("Scala Tools Nexus" at nexus + "snapshots/")
  else Some("Scala Tools Nexus" at nexus + "releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
