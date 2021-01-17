val dottyVersion = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    useScala3doc := true,
    libraryDependencies ++= {
      Seq(
        // Dependencies native to Scala 3
        "com.novocode" % "junit-interface" % "0.11" % "test"
      ) ++ Seq(
        // Dependencies requiring Scala 2 compatible mode
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
        "org.hamcrest" % "hamcrest" % "2.2" % Test
        // "org.scalaz" %% "scalaz-core" % "7.3.2"
      ).map(_.withDottyCompat(scalaVersion.value))
    }
  )
