val dottyVersion = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "io.github.rami3l.yascm",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= {
      Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
        "dev.optics" %% "monocle-core" % "3.1.0",
        "dev.optics" %% "monocle-macro" % "3.1.0",
        "org.typelevel" %% "cats-core" % "2.7.0",
        "org.typelevel" %% "cats-effect" % "3.3.7",
        // Testing
        "org.scalatest" %% "scalatest" % "3.2.11" % "test",
        "org.typelevel" %% "cats-effect-testing-scalatest" % "1.4.0" % Test
      )
    }
  )
