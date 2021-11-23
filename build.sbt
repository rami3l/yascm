val dottyVersion = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= {
      Seq(
        "com.novocode" % "junit-interface" % "0.11" % "test",
        "org.hamcrest" % "hamcrest" % "2.2" % Test,
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
      )
    }
  )
