package com.example

import cats.Semigroup
import munit.CatsEffectSuite
import cats.implicits._

class CatsSuite extends CatsEffectSuite {
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

    final case class Price(value: Int)

    object Price {
      implicit val sum: Semigroup[Price] = (p1, p2) => Price(p1.value + p2.value)
    }

    assertEquals(Price(1) |+| Price(2), Price(3))
  }
}
