val dottyVersion = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
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
        "com.novocode" % "junit-interface" % "0.11" % "test",
        "org.hamcrest" % "hamcrest" % "2.2" % Test
      )
    }
  )
