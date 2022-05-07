ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect"         % "3.3.11",
  "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
)

lazy val root = (project in file("."))
  .settings(
    name.withRank(KeyRanks.Invisible) := "ProjectScala2",
    idePackagePrefix.withRank(KeyRanks.Invisible) := Some("com.example")
  )
