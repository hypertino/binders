import sbt.Keys._

crossScalaVersions := Seq("2.11.8", "2.10.6")

lazy val binders = crossProject.settings(shared:_*).settings(
    name := "binders",
    version := "1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.hypertino" %%% "inflector" % "1.0-SNAPSHOT",
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    )
  )
  .jsSettings(
    // JS-specific settings here
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.mockito" % "mockito-all" % "1.10.19" % "test"
    )
  )

lazy val js = binders.js

lazy val jvm = binders.jvm

lazy val perftest = crossProject.settings(shared:_*).settings(
    name := "perftest")
  .enablePlugins(JmhPlugin)

val shared = Seq(
  organization := "com.hypertino",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ) ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
          "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary)
      case _ ⇒ Seq.empty
    }
  }
)
