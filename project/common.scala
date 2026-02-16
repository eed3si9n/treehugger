import sbt._
import Keys._

object Common {

  val docSettings: Seq[Def.Setting[_]] = Seq(
    // scaladoc fix
    unmanagedClasspath in Compile += Attributed.blank(file("doesnotexist"))

    // scaladoc for 2.8.1 keeps failing, so run package-doc manually, copy it to `scaladoc` folder
    // and uncomment the following
    // packageDoc in Compile <<= (scalaVersion, baseDirectory, name, version) map { (sv, base, name, v) =>
    //  base / "javadoc" / (name + "_" + sv + "-" + v + "-javadoc.jar")
    // },
    // packageDoc in Test <<= (scalaVersion, baseDirectory, name, version) map { (sv, base, name, v) =>
    //  base / "javadoc" / (name + "_" + sv + "-" + v + "-javadoc.jar")
    // }
  )

  val sonatypeSettings: Seq[Def.Setting[_]] = Seq(
    pomExtra := (<scm>
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
    publishMavenStyle                        := true,
    publishArtifact in (Compile, packageBin) := true,
    publishArtifact in Test                  := false,
    publishTo                                := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("staging" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false }
  )
}
