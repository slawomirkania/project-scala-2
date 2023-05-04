package com.example

import munit.CatsEffectSuite

import scala.math.BigDecimal.RoundingMode

class ScalaSuite extends CatsEffectSuite {
  test("Type class") {
    final case class Square(a: Double)
    final case class Rectangle(a: Double, b: Double)
    final case class Circle(r: Double)

    trait Area[S] {
      def calculate(s: S): Double
    }

    object Area {
      private def halfUp(d: Double) = BigDecimal.apply(d).setScale(2, RoundingMode.HALF_UP).doubleValue

      implicit val squareAreaCalculator: Area[Square]       = (s: Square) => halfUp(Math.pow(s.a, 2.0))
      implicit val rectangleAreaCalculator: Area[Rectangle] = (s: Rectangle) => halfUp(s.a * s.b)
      implicit val circleAreaCalculator: Area[Circle]       = (s: Circle) => halfUp(s.r * Math.PI)
    }

    implicit class AreaOps[S: Area](s: S) {
      def area: Double = implicitly[Area[S]].calculate(s)
    }

    assertEquals(Square(5.2).area, 27.04)
    assertEquals(Rectangle(5.2, 2.54).area, 13.21)
    assertEquals(Circle(2.3).area, 7.23)
  }

  test("Variance") {
    trait Animal
    final case class Dog() extends Animal
    final case class Cat() extends Animal

    class UniversalCage[+T] // covariant
    class DedicatedCage[T]  // invariant
    class Vet[-T]           // contravariant

    val catCage: UniversalCage[Animal]       = new UniversalCage[Cat]
    val dogCage: UniversalCage[Animal]       = new UniversalCage[Dog]
    val dogDedicatedCage: DedicatedCage[Dog] = new DedicatedCage[Dog]
    val catDedicatedCage: DedicatedCage[Cat] = new DedicatedCage[Cat]
    val catVet: Vet[Cat]                     = new Vet[Animal]
    val dogVet: Vet[Dog]                     = new Vet[Animal]
  }

  test("Currying") {
    def sum(a: Int, b: Int): Int      = a + b
    def curriedSum: Int => Int => Int = a => b => a + b

    assertEquals(sum(2, 4), 6)
    assertEquals(curriedSum(2)(4), 6)

    // eta-expansion
    assertEquals((sum _).curried(2)(4), 6)

    // partial application
    assertEquals(Some(2).map(curriedSum(4)), Some(6))
  }

  test("Partially applied functions") {
    def sum(a: Int, b: Int): Int        = a + b
    def partiallyAppliedSum: Int => Int = sum(1, _: Int)

    assertEquals(partiallyAppliedSum(2), 3)
  }
}
