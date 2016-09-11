organization in ThisBuild := "com.github.fdietze"
name in ThisBuild := "pharg"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(phargJS, phargJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val pharg = (crossProject.crossType(CrossType.Pure) in file("."))
  .settings(
    libraryDependencies ++= (
      "org.specs2" %% "specs2-core" % "3.8.4" % "test" ::
      Nil
    ),

    scalacOptions in Test ++= Seq("-Yrangepos"), // for Specs2

    // scalaxy (faster collection operations)
    scalacOptions += "-Xplugin-require:scalaxy-streams",
    scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams")),
    scalacOptions in Test += "-Xplugin-disable:scalaxy-streams",
    autoCompilerPlugins := true,
    addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4"),

    scalacOptions ++= (
      "-encoding" :: "UTF-8" ::
      "-unchecked" ::
      "-deprecation" ::
      "-explaintypes" ::
      "-feature" ::
      "-language:_" ::
      "-Xlint:_" ::
      "-Ywarn-unused" ::
      // "-Xdisable-assertions" ::
      // "-optimize" ::
      // "-Yopt:_" :: // enables all 2.12 optimizations
      // "-Yinline" :: "-Yinline-warnings" ::
      Nil
    )
  )

lazy val phargJVM = pharg.jvm
lazy val phargJS = pharg.js
