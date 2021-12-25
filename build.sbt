lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-basic-scala3",
    version := "0.1.0",
    scalaVersion := "3.1.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.27" % Test
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
