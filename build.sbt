val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test"
      // "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
      // "org.scalaz" %% "scalaz-core" % "7.3.2"
    )
  )
