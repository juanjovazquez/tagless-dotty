//val dottyVersion = dottyLatestNightlyBuild.get
val dottyVersion = "0.24.0-RC1"

lazy val `tagless-scala` = project
  .in(file("."))
  .settings(
    scalaVersion := dottyVersion,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-language:implicitConversions"
    ),
    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-staging" % dottyVersion,
      ("org.typelevel" %% "cats-core" % "2.1.1").withDottyCompat(dottyVersion),
      ("org.typelevel" %% "cats-effect" % "2.1.3").withDottyCompat(dottyVersion),
      "org.scalameta" %% "munit" % "0.7.4" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
