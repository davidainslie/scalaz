import Dependencies._
import sbt._

lazy val root = project("scalaz", file("."))
  .settings(description := "Scalaz by Backwards")
  .settings(javaOptions in Test ++= Seq("-Dconfig.resource=application.test.conf"))

lazy val copyDocAssetsTask = taskKey[Unit]("Copy unidoc resources")

copyDocAssetsTask := {
  println("Copying unidoc resources")
  val sourceDir = file("src/main/doc-resources")
  val targetDir = (target in (Compile, doc)).value.getParentFile
  println(s"from ${sourceDir.getAbsolutePath} to ${targetDir.getAbsolutePath}")
  IO.copyDirectory(sourceDir, new java.io.File(targetDir, "unidoc"))
}

copyDocAssetsTask := (copyDocAssetsTask triggeredBy (unidoc in Compile)).value
def project(id: String, base: File): Project =
  Project(id, base)
    .enablePlugins(JavaAppPackaging, ScalaUnidocPlugin, JavaUnidocPlugin)
    .settings(
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots"),
        Resolver.bintrayRepo("cakesolutions", "maven"),
        "Artima Maven Repository" at "http://repo.artima.com/releases",
        "jitpack" at "https://jitpack.io",
        "Confluent Platform Maven" at "http://packages.confluent.io/maven/"
      ),
      scalaVersion := BuildProperties("scala.version"),
      sbtVersion := BuildProperties("sbt.version"),
      organization := "com.backwards",
      name := id,
      autoStartServer := false,
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      libraryDependencies ++= dependencies,
      scalacOptions ++= Seq(
        "-encoding", "utf8",
        "-deprecation",
        "-unchecked",
        "-language:implicitConversions",
        "-language:higherKinds",
        "-language:existentials",
        "-language:postfixOps",
        "-Ywarn-value-discard",
        "-Xfatal-warnings"
      ),
      fork := true,
      publishArtifact in Test := true
    )