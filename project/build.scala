import sbt._

object Builds extends Build {
  import Keys._
  import ls.Plugin.{LsKeys => lskeys, _}

  lazy val buildSettings = Defaults.defaultSettings ++ lsSettings ++ Seq(
    organization := "com.eed3si9n",
    version := "0.2.3",
    scalaVersion := "2.10.1",
    crossScalaVersions := Seq("2.10.1", "2.9.2", "2.9.1", "2.9.0-1"),
    homepage := Some(url("http://eed3si9n.com/treehugger")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
    description := "a library to code Scala programmatically.",
    initialCommands in console := """import treehugger.forest._
                                    |import definitions._
                                    |import treehuggerDSL._""".stripMargin,
    
    lskeys.tags in lskeys.lsync := Seq("code-generation"),

    // scaladoc fix
    unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist")),

    // scaladoc for 2.8.1 keeps failing, so run package-doc manually, copy it to `scaladoc` folder
    // and uncomment the following
    // packageDoc in Compile <<= (scalaVersion, baseDirectory, name, version) map { (sv, base, name, v) =>
    //  base / "javadoc" / (name + "_" + sv + "-" + v + "-javadoc.jar")
    // },
    // packageDoc in Test <<= (scalaVersion, baseDirectory, name, version) map { (sv, base, name, v) =>
    //  base / "javadoc" / (name + "_" + sv + "-" + v + "-javadoc.jar")
    // },

    resolvers ++= Seq("sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases")
  )

  lazy val publishSettings = Seq(
    pomExtra :=
      (<scm>
        <url>git@github.com:eed3si9n/treehugger.git</url>
        <connection>scm:git:git@github.com:eed3si9n/treehugger.git</connection>
      </scm>
      <developers>
        <developer>
          <id>eed3si9n</id>
          <name>Eugene Yokota</name>
          <url>http://eed3si9n.com</url>
        </developer>
      </developers>),
    publishMavenStyle := true,
    publishArtifact in (Compile, packageBin) := true,
    publishArtifact in Test := false,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) 
        Some("snapshots" at nexus + "content/repositories/snapshots") 
      else
        Some("staging"  at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    crossVersion <<= scalaVersion { sv =>
      ("-(M|RC)".r findFirstIn sv) map {_ => CrossVersion.full} getOrElse CrossVersion.binary
    }
  )

  lazy val root = Project("root", file("."),
    settings = buildSettings ++ Seq(name := "treehugger"))
  lazy val library = Project("library", file("library"),
    settings = buildSettings ++ publishSettings ++ Seq(
      name := "treehugger",
      libraryDependencies <++= (scalaVersion) { (sv) => sv match {
        case "2.8.1"   => Seq("org.specs2" %% "specs2" % "1.5" % "test",
                              "org.specs2" %% "specs2-scalaz-core" % "5.1-SNAPSHOT" % "test")
        case "2.9.0-1" | "2.9.1" | "2.9.2" =>
          Seq("org.specs2" % "specs2_2.9.1" % "1.6.1" % "test",
              "org.specs2" % "specs2-scalaz-core_2.9.1" % "6.0.RC2" % "test")
        case "2.10.0" => Seq("org.specs2" % "specs2_2.10" % "1.14" % "test")
        case _ =>         Seq("org.specs2" %% "specs2" % "1.14" % "test")
      }}
    ))
  lazy val swing = Project("treehugger_bridge", file("bridge"),
    settings = buildSettings ++ publishSettings ++ Seq(

    )) dependsOn(library)
}
