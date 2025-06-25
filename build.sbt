import org.typelevel.sbt.tpolecat.DevMode

import Dependencies.*

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
  .aggregate(
    proustCommon,
    proustPropositional,
    proustPredicate
  )

/**
  * Chapter 2
  */
lazy val proustPropositional = project
  .in(file("modules/proust-propositional"))
  .dependsOn(proustCommon)
  .settings(
    libraryDependencies ++= Seq(
      catsEffect,
      catsMtl,
      kittens,
      weaver % Test
    )
  )

/**
  * Chapter 3
  */
lazy val proustPredicate = project
  .in(file("modules/proust-predicate"))
  .dependsOn(proustCommon)
  .settings(
    libraryDependencies ++= Seq(
      weaver % Test
    )
  )

/**
  * Common definitions and utilities
  */
lazy val proustCommon = project
  .in(file("modules/proust-common"))
  .settings(
    libraryDependencies ++= Seq(
      cats,
      catsParse,
      iron
    )
  )
