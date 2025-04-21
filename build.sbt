import org.typelevel.sbt.tpolecat.DevMode

inThisBuild(
  Seq(
    scalaVersion               := "3.6.4",
    tpolecatDefaultOptionsMode := DevMode,
    semanticdbEnabled          := true,
    semanticdbVersion          := scalafixSemanticdb.revision
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(proust)

lazy val proust = project
  .in(file("modules/proust"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"       %% "cats-core"   % "2.13.0",
      "org.typelevel"       %% "cats-effect" % "3.6.1",
      "org.typelevel"       %% "cats-parse"  % "1.1.0",
      "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test
    )
  )
