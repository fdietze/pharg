organization in ThisBuild := "com.github.fdietze"
name in ThisBuild := "pharg"
version in ThisBuild := "0.1.1"

scalaVersion in ThisBuild := "2.12.1"

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8", "2.12.1")

lazy val root = project.in(file(".")).
  aggregate(phargJS, phargJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val pharg = (crossProject.crossType(CrossType.Pure) in file("."))
  .settings(
    libraryDependencies ++= (
      "org.specs2" %% "specs2-core" % "3.8.6" % "test" ::
      "org.typelevel" %%% "cats" % "0.8.1" ::
      Nil
    ),

    scalacOptions in Test ++= Seq("-Yrangepos"), // for Specs2

    //TODO: wartremover

    initialCommands in console := """
    import pharg._
    import pharg.DSL._
    """,

    scalacOptions ++= (
      "-unchecked" ::
      "-deprecation" ::
      "-explaintypes" ::
      "-feature" ::
      "-language:_" ::
      // "-Xlint:_" ::
      // "-Ywarn-unused" ::
      // "-Xdisable-assertions" ::
      // "-optimize" ::
      // "-Yopt:_" :: // enables all 2.12 optimizations
      // "-Yinline" :: "-Yinline-warnings" ::
      Nil
    )
  )

lazy val phargJVM = pharg.jvm
lazy val phargJS = pharg.js

pgpSecretRing in Global := file("secring.gpg")
pgpPublicRing in Global := file("pubring.gpg")
pgpPassphrase in Global := Some("".toCharArray)

organization in Global := "com.github.fdietze"

pomExtra in Global := {
  <url>https://github.com/fdietze/pharg</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/fdietze/pharg</url>
    <connection>scm:git:git@github.com:fdietze/pharg.git</connection>
  </scm>
  <developers>
    <developer>
      <id>fdietze</id>
      <name>Felix Dietze</name>
      <url>https://github.com/fdietze</url>
    </developer>
  </developers>
}
