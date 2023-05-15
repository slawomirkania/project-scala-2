package com.example

import cats.effect.{ IO, Resource }
import munit.{ CatsEffectSuite, ScalaCheckEffectSuite }
import doobie._
import doobie.implicits._
import doobie.h2._
import org.scalacheck.{ Gen, ShrinkLowPriority }
import org.scalacheck.effect.PropF.forAllF

class DoobieSuite extends CatsEffectSuite with ScalaCheckEffectSuite with ShrinkLowPriority {
  import DoobieSuite._

  test("Check connection") {
    val program = for {
      a <- sql"SELECT 42".query[Int].unique
      b <- sql"SELECT 43".query[Int].unique
    } yield (a, b)

    assertIO(H2Store.commit(program), (42, 43))
  }

  test("Check query") {
    forAllF(Gen.alphaUpperStr.retryUntil(_.length <= 3), Gen.alphaUpperStr, Gen.choose(1, 10000)) {
      (code, name, population) =>
        val program = for {
          _     <- Queries.dropTable.update.run
          _     <- Queries.createCountryTable.update.run
          _     <- Queries.insertData.update.run
          _     <- sql"INSERT INTO country VALUES ($code, $name, $population)".update.run
          count <- sql"SELECT count(*) FROM country".query[Int].unique
        } yield count

        assertIO(H2Store.commit(program), 3)
    }
  }

  test("read row to case class") {
    forAllF(Gen.alphaUpperStr.retryUntil(_.length <= 3), Gen.alphaUpperStr, Gen.choose(1, 10000)) {
      (code, name, population) =>
        final case class Full(code: String, name: String, population: Long)
        final case class Part(code: String, population: Long)

        final case class Combined(part: Part, name: String)

        val fullExpected     = Full(code, name, population)
        val partExpected     = Part(code, population)
        val combinedExpected = Combined(partExpected, name)

        val program = for {
          _        <- Queries.dropTable.update.run
          _        <- Queries.createCountryTable.update.run
          _        <- sql"INSERT INTO country VALUES ($code, $name, $population)".update.run
          full     <- sql"SELECT code, name, population FROM country".query[Full].unique
          part     <- sql"SELECT code, population FROM country".query[Part].unique
          combined <- sql"SELECT code, population, name FROM country".query[Combined].unique
        } yield (full, part, combined)

        assertIO(H2Store.commit(program), (fullExpected, partExpected, combinedExpected))
    }
  }
}

object DoobieSuite {
  object H2Store {
    def commit[A](txn: ConnectionIO[A]): IO[A] = transactor.use(xa => txn.transact(xa))

    val transactor: Resource[IO, H2Transactor[IO]] =
      for {
        ce <- ExecutionContexts.fixedThreadPool[IO](32)
        xa <- H2Transactor.newH2Transactor[IO](
          "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
          "sa",
          "",
          ce
        )
      } yield xa
  }

  object Queries {
    val createCountryTable =
      sql"""
           |CREATE TABLE country (
           |  code       character(3)  NOT NULL,
           |  name       text          NOT NULL,
           |  population integer       NOT NULL
           |)
           |""".stripMargin

    val insertData =
      sql"""
           |INSERT INTO country VALUES ('PL', 'Poland', 40000000);
           |INSERT INTO country VALUES ('US', 'USA', 400000000);
           |""".stripMargin

    val dropTable =
      sql"""DROP TABLE IF EXISTS country;"""
  }
}
