ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= Seq(
  Dependency.catsEffect
) ++ Seq(
  Dependency.doobie,
  Dependency.doobieH2
) ++ Seq(
  Dependency.munitCatsEffect,
  Dependency.munitScalaCheckEffect
) ++ Seq(
  Dependency.http4sCore,
  Dependency.http4sDsl,
  Dependency.http4sCirce,
  Dependency.http4sClient,
  Dependency.http4sServer
)

lazy val root = (project in file("."))
  .settings(
    name.withRank(KeyRanks.Invisible) := "ProjectScala2",
    idePackagePrefix.withRank(KeyRanks.Invisible) := Some("com.example")
  )
