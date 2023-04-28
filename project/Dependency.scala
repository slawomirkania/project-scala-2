import sbt.*

object Dependency {
  object Version {
    val doobie        = "1.0.0-RC1"
    val http4sVersion = "0.23.18"
    val fs2           = "3.6.1"
  }

  val catsEffect = "org.typelevel" %% "cats-effect" % "3.4.8"

  val doobie   = "org.tpolecat" %% "doobie-core" % Version.doobie
  val doobieH2 = "org.tpolecat" %% "doobie-h2"   % Version.doobie

  val http4sCore   = "org.http4s" %% "http4s-core"   % Version.http4sVersion
  val http4sDsl    = "org.http4s" %% "http4s-dsl"    % Version.http4sVersion
  val http4sCirce  = "org.http4s" %% "http4s-circe"  % Version.http4sVersion
  val http4sClient = "org.http4s" %% "http4s-client" % Version.http4sVersion
  val http4sServer = "org.http4s" %% "http4s-server" % Version.http4sVersion

  val fs2Core = "co.fs2" %% "fs2-core" % Version.fs2
  val fs2IO   = "co.fs2" %% "fs2-io"   % Version.fs2

  val munitCatsEffect       = "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7" % Test
  val munitScalaCheckEffect = "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test
}
