import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val scala213 = "2.13.1"
lazy val scala212 = "2.12.10"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List(scala213, scala212, scala211)

ThisBuild / scalaVersion := scala213

ThisBuild / organization := "com.hypertino"

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation")

lazy val binders = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(publishSettings:_*)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    name := "binders",
    version := "1.2-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.hypertino" %%% "inflector" % "1.0.13",
      "org.scalamock" %%% "scalamock" % "4.4.0" % Test,
      "org.scalatest" %% "scalatest" % "3.1.0" % Test,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    publishArtifact := true,
    publishArtifact in Test := false,
    resolvers ++= Seq(
      Resolver.sonatypeRepo("public")
    )
  )
  .jsSettings(
  )
  .jvmSettings(
  )

lazy val js = binders.js

lazy val jvm = binders.jvm

lazy val `binders-root` = project.settings(publishSettings:_*).in(file("."))
  .settings(publishSettings:_*)
  .aggregate(js, jvm)
  .settings(
    crossScalaVersions := Nil,
    publish / skip := true
  )

val publishSettings = Seq(
  pomExtra := <url>https://github.com/hypertino/binders</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://opensource.org/licenses/BSD-3-Clause</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:hypertino/binders.git</url>
      <connection>scm:git:git@github.com:hypertino/binders.git</connection>
    </scm>
    <developers>
      <developer>
        <id>maqdev</id>
        <name>Magomed Abdurakhmanov</name>
        <url>https://github.com/maqdev</url>
      </developer>
      <developer>
        <id>hypertino</id>
        <name>Hypertino</name>
        <url>https://github.com/hypertino</url>
      </developer>
    </developers>,
  pgpSecretRing := file("./travis/script/ht-oss-private.asc"),
  pgpPublicRing := file("./travis/script/ht-oss-public.asc"),
  usePgpKeyHex("F8CDEF49B0EDEDCC"),
  pgpPassphrase := Option(System.getenv().get("oss_gpg_passphrase")).map(_.toCharArray),
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false},
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  credentials ++= (for {
    username <- Option(System.getenv().get("sonatype_username"))
    password <- Option(System.getenv().get("sonatype_password"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

