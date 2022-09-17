val dottyVersion = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "io.github.rami3l.yascm",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    scalacOptions ++= Seq(
      // https://docs.scala-lang.org/scala3/reference/experimental/explicit-nulls.html
      "-Yexplicit-nulls"
    ),
    libraryDependencies ++= {
      Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
        "dev.optics" %% "monocle-core" % "3.1.0",
        "dev.optics" %% "monocle-macro" % "3.1.0",
        "org.typelevel" %% "cats-core" % "2.8.0",
        "org.typelevel" %% "cats-effect" % "3.4-389-3862cf0",
        // Testing
        "org.scalatest" %% "scalatest" % "3.2.13" % "test",
        "org.typelevel" %% "cats-effect-testing-scalatest" % "1.4.0" % Test
      )
    }
  )
