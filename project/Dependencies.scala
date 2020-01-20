import sbt._

object Dependencies {
  lazy val dependencies: Seq[ModuleID] =
    Seq(
      scalatest, scalacheck, scalacheckShapeless, pureConfig,
      monocle, scalaz, contextual, refined
    ).flatten
  
  lazy val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.1.0" % Test withSources() withJavadoc()
  )

  lazy val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck" % "1.14.3" % Test withSources() withJavadoc()
  )

  lazy val scalacheckShapeless: Seq[ModuleID] = Seq(
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" withSources() withJavadoc()
  )

  lazy val pureConfig: Seq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.12.2"

    Seq("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: Seq[ModuleID] = {
    val group = "com.github.julien-truffaut"
    val version = "2.0.0"

    Seq(
      "monocle-law"
    ).map(group %% _ % version % Test withSources() withJavadoc()) ++ Seq(
      "monocle-core", "monocle-macro", "monocle-generic"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val scalaz: Seq[ModuleID] = {
    val group = "org.scalaz"
    val version = "7.2.30"

    Seq(
      "scalaz-core"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "scalaz-ioeffect"
    ).map(group %% _ % "2.10.1" withSources())
  }

  lazy val contextual: Seq[ModuleID] = Seq(
    "com.propensive" %% "contextual" % "1.2.1" withSources() withJavadoc()
  )

  lazy val refined: Seq[ModuleID] = Seq(
    "eu.timepit" %% "refined-scalaz" % "0.9.2"
  )
}