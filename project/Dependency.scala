import sbt.*

object Dependency {
  object Version {
    val doobie = "1.0.0-RC1"
  }

  val catsEffect = "org.typelevel" %% "cats-effect" % "3.4.8"
  val doobie     = "org.tpolecat"  %% "doobie-core" % Version.doobie
  val doobieH2   = "org.tpolecat"  %% "doobie-h2"   % Version.doobie

  val munitCatsEffect       = "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7" % Test
  val munitScalaCheckEffect = "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test
}
