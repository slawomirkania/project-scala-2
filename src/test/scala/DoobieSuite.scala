package com.example

import cats.effect.{ IO, Resource }
import munit.CatsEffectSuite
import doobie._
import doobie.implicits._
import doobie.h2._
import org.scalacheck.Gen
import org.scalacheck.effect.PropF.forAllF

class DoobieSuite extends CatsEffectSuite {
  import DoobieSuite._

  test("Check connection") {
    val program = for {
      a <- sql"SELECT 42".query[Int].unique
      b <- sql"SELECT 43".query[Int].unique
    } yield (a, b)

    assertIO(H2Store.commit(program), (42, 43))
  }

  test("Check query") {
    forAllF(Gen.alphaUpperStr.retryUntil(_.length == 3), Gen.alphaUpperStr, Gen.choose(1, 10000)) {
      (code, name, population) =>
        val program = for {
          _     <- Queries.createCountryTable.update.run
          _     <- Queries.insertData.update.run
          _     <- sql"INSERT INTO country values ('$code', '$name', $population)".update.run
          count <- sql"SELECT count(*) FROM country".query[Int].unique
        } yield count

        assertIO(H2Store.commit(program), 3)
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
           |INSERT INTO country values ('PL', 'Poland', 40000000);
           |INSERT INTO country values ('US', 'USA', 400000000);
           |""".stripMargin
  }
}
