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

  test("Get and Read") {
    def sql = sql"select 1,2,3"

    val program = for {
      tupledInts    <- sql.query[(Int, Int, Int)].unique
      tupledClasses <- sql.query[(One, Two, Incremented)].unique
      numbers       <- sql.query[Numbers].unique
    } yield (tupledInts, tupledClasses, numbers)

    assertIO(
      H2Store.commit(program),
      ((1, 2, 3), (One(1), Two(2), Incremented(3)), Numbers(One(11), Two(12), Incremented(14)))
    )
  }

  test("Write") {
    val numbers = Numbers(One(1), Two(2), Incremented(3))

    val program = for {
      _      <- Queries.dropTable.update.run
      _      <- Queries.createNumbersTable.update.run
      _      <- sql"INSERT INTO numbers VALUES ($numbers)".update.run
      result <- sql"SELECT one, two, incremented FROM numbers".query[(Int, Int, Int)].unique
    } yield result

    assertIO(H2Store.commit(program), (1, 2, 4))
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

    val createNumbersTable =
      sql"""
           |CREATE TABLE numbers (
           |  one         integer   NOT NULL,
           |  two         integer   NOT NULL,
           |  incremented integer   NOT NULL
           |)
           |""".stripMargin

    val insertData =
      sql"""
           |INSERT INTO country VALUES ('PL', 'Poland', 40000000);
           |INSERT INTO country VALUES ('US', 'USA', 400000000);
           |""".stripMargin

    val dropTable =
      sql"""DROP TABLE IF EXISTS country; DROP TABLE IF EXISTS numbers;"""
  }

  final case class Numbers(one: One, two: Two, incremented: Incremented)
  final case class One(value: Int)         extends AnyVal
  final case class Two(value: Int)         extends AnyVal
  final case class Incremented(value: Int) extends AnyVal

  object Numbers {
    implicit val getOne: Get[One]                 = Get[Int].temap(i => Either.cond(i == 1, One(i), "Error parsing int to One"))
    implicit val getTwo: Get[Two]                 = Get[Int].temap(i => Either.cond(i == 2, Two(i), "Error parsing int to Two"))
    implicit val getIncremented: Get[Incremented] = Get[Int].tmap(i => Incremented(i + 1))
    implicit val readNumbersAdd10: Read[Numbers] = Read[(One, Two, Incremented)].map { case (one, two, incremented) =>
      Numbers(One(one.value + 10), Two(two.value + 10), Incremented(incremented.value + 10))
    }
    implicit val writeNumbersIncremented: Write[Numbers] = Write[(One, Two, Incremented)].contramap { n =>
      (n.one, n.two, Incremented(n.incremented.value + 1))
    }
  }
}
