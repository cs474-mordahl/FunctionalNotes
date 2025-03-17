val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name                   := "AlgebraicDataTypes",
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
      "-source:future"
    ),

    // Iron for strong type constraints
    libraryDependencies += "io.github.iltotore" % "iron_3" % "2.6.0",

    // Test Dependencies
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test
  )
