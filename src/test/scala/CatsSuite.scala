package com.example

import cats.{ Apply, Functor, Monoid, Semigroup }
import munit.CatsEffectSuite
import cats.implicits._
import cats.kernel.Eq
import cats.kernel.laws.discipline.{ MonoidTests, SemigroupTests }
import munit.DisciplineSuite

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

  import org.scalacheck.ScalacheckShapeless._

  checkAll("Price.Semigroup", SemigroupTests[Price].semigroup)
  checkAll("OptionalPrice.MonoidLaws", MonoidTests[OptionalPrice].monoid)

  test("Functor") {
    assertEquals(Functor[Option].map(Option("test"))(_.length), Option(4))

    val lengthFunctor: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    assertEquals(lengthFunctor(Option("test")), Option(4))

    assertEquals(Functor[Hole].map(Hole(5))(_ * 2), Hole(10))
    assertEquals(Hole(5).map(_ * 2), Hole(10))

    val listOption    = Functor[List].compose(Functor[Option])
    val listOfOptions = List(Some(1), Some(2), None, Some(3))

    assertEquals(listOption.map(listOfOptions)(_ + 1), List(Some(2), Some(3), None, Some(4)))

    val optionList   = Functor[Option].compose(Functor[List])
    val optionOfList = Option(List(Some(1), Some(2), None, Some(3)))

    assertEquals(optionList.map(optionOfList)(_.map(_ + 1)), Some(List(Some(2), Some(3), None, Some(4))))

    val names = List("asd", "dsadfsa", "d")
    assertEquals(Functor[List].fproduct(names)(_.length).toMap, Map("asd" -> 3, "dsadfsa" -> 7, "d" -> 1))
  }

  test("Apply") {
    assertEquals(Apply[Option].ap[String, Int](Some(_.length))(Some("test")), Some(4))
    assertEquals(
      Apply[List].ap[String, String](List(_ + "_", _ + "*"))(List("test1", "test2")),
      List("test1_", "test2_", "test1*", "test2*")
    )

    assertEquals(Apply[Option].ap2[Int, Int, Int](Some((a, b) => a + b))(Some(1), Some(2)), Some(3))
    assertEquals((Option(1), Option(2)).apWith[Int](Option((a, b) => a + b)), Option(3))

    assertEquals(Apply[Option].map2(Some(1), Some(2))((a, b) => a + b), Some(3))
    assertEquals((Option(1), Option(2)).mapN((a, b) => a + b), Some(3))

    assertEquals(Apply[Option].tuple2(Some(1), Some(2)), Some((1, 2)))
  }
}

trait CatsSuiteContext {
  final case class Hole[A](value: A)

  object Hole {
    implicit val functorHole: Functor[Hole] = new Functor[Hole] {
      override def map[A, B](fa: Hole[A])(f: A => B): Hole[B] = Hole(f(fa.value))
    }
  }

  final case class Price(value: Int)

  object Price {
    implicit def eqPrice: Eq[Price]    = Eq.fromUniversalEquals
    implicit def sum: Semigroup[Price] = (p1, p2) => Price(p1.value + p2.value)
  }

  final case class OptionalPrice(value: Option[Int])

  object OptionalPrice {
    implicit def eqOptionalPrice: Eq[OptionalPrice] = Eq.fromUniversalEquals
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
