package com.example

import cats.{ Monoid, Semigroup }
import munit.CatsEffectSuite
import cats.implicits._
import cats.kernel.Eq
import cats.kernel.laws.discipline.{ MonoidTests, SemigroupTests }
import munit.DisciplineSuite
import org.scalacheck.{ Arbitrary, Gen }

class CatsSuite extends CatsEffectSuite with CatsSuiteContext with DisciplineSuite {

  test("Semigroup") {
    assertEquals(Semigroup[Int].combine(1, 2), 3)
    assertEquals(Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)), List(1, 2, 3, 4, 5, 6))
    assertEquals(Semigroup[Option[Int]].combine(Some(1), Some(2)), Some(3))
    assertEquals(Semigroup[Option[Int]].combine(Some(1), None), Some(1))
    assertEquals(Semigroup[String].combine("a", "b"), "ab")
    assertEquals(Semigroup[String].combineN("a", 4), "aaaa")
    assertEquals(Semigroup[Int => Int].combine(_ + 2, _ * 10)(6), 68)

    assertEquals(Semigroup[Int].combineAllOption(List(1, 2, 3)), Some(6))
    assertEquals(Semigroup[Int].combineAllOption(List.empty), None)

    assertEquals(1 |+| 2, 3)
    assertEquals(Option(1) |+| Option(2), Some(3))

    assertEquals(Price(1) |+| Price(2), Price(3))
  }

  test("Monoid") {
    assertEquals(Monoid[Int].empty, 0)
    assertEquals(Monoid[String].empty, "")
    assertEquals(Monoid[List[Int]].empty, Nil)
    assertEquals(Monoid[Option[Int]].empty, None)
    assertEquals(Monoid[Int].combineAll(List(1, 2)), 3)
    assertEquals(Monoid[Int].combineAll(List.empty), 0)

    val l = List(1, 2, 3, 4, 5)
    assertEquals(l.foldMap(identity), 15)
    assertEquals(l.foldMap(_.toString), "12345")

    import OptionalPrice._

    assertEquals(Monoid[OptionalPrice].empty, OptionalPrice(Some(0)))

    val optionalPrices  = List(OptionalPrice(Some(1)), OptionalPrice(Some(2)))
    val optionalPrices2 = List(OptionalPrice(Some(1)), OptionalPrice(Some(2)), OptionalPrice(None))

    assertEquals(optionalPrices.foldMap(identity), OptionalPrice(Some(3)))
    assertEquals(optionalPrices2.foldMap(identity), OptionalPrice(None))
  }

  checkAll("Price.Semigroup", SemigroupTests[Price].semigroup)
  checkAll("OptionalPrice.MonoidLaws", MonoidTests[OptionalPrice].monoid)
}

trait CatsSuiteContext {
  final case class Price(value: Int)

  object Price {
    implicit def eqPrice: Eq[Price]         = Eq.fromUniversalEquals
    implicit def arbPrice: Arbitrary[Price] = Arbitrary(Gen.posNum[Int].map(Price.apply))
    implicit def sum: Semigroup[Price]      = (p1, p2) => Price(p1.value + p2.value)
  }

  final case class OptionalPrice(value: Option[Int])

  object OptionalPrice {
    implicit def eqOptionalPrice: Eq[OptionalPrice] = Eq.fromUniversalEquals
    implicit def arbOptionalPrice: Arbitrary[OptionalPrice] = Arbitrary(
      Gen.option(Gen.posNum[Int]).map(OptionalPrice.apply)
    )
    implicit def monoidOptionalPrice: Monoid[OptionalPrice] =
      new Monoid[OptionalPrice] {
        def combine(x: OptionalPrice, y: OptionalPrice): OptionalPrice = {
          val r = for {
            xval <- x.value
            yval <- y.value
          } yield xval + yval

          OptionalPrice(r)
        }

        def empty: OptionalPrice = OptionalPrice(Some(0))
      }
  }
}
