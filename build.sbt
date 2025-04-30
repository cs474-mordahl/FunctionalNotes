val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name                   := "FunctionalNotes",
    version                := "1.0.0",
    scalaVersion           := scala3Version,
    semanticdbEnabled      := true,
    semanticdbVersion      := scalafixSemanticdb.revision,
    Compile / doc / target := file("docs"),
    scalacOptions ++= Seq(
      "-Wunused:all",
      "-Wnonunit-statement",
      "-Wvalue-discard",
      "-deprecation",
      "-feature",
      "-source:future",
      "-Vprint:typer"
    ),
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6.1",
    libraryDependencies += "org.typelevel" %% "cats-core"   % "2.13.0",
    libraryDependencies += "org.typelevel" %% "kittens"     % "3.5.0",
    libraryDependencies += "com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.16",

    // Test Dependencies
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test
  )
