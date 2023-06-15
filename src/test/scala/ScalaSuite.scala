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

  test("Partial function") {
    def incrementEven: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
      def isDefinedAt(x: Int): Boolean = x % 2 == 0
      def apply(x: Int): Int           = x + 1
    }

    assertEquals(incrementEven.isDefinedAt(1), false)
    assertEquals(incrementEven.isDefinedAt(2), true)
    assertEquals(incrementEven.isDefinedAt(4), true)

    def incrementEvenImplicit: PartialFunction[Int, Int] = {
      case x if x % 2 == 0 => x + 1
    }

    // assertEquals(evenImplicit(1), 2) // scala.MatchError
    assertEquals(incrementEvenImplicit(2), 3)
    assertEquals(incrementEvenImplicit(4), 5)

    def incrementOddImplicit: PartialFunction[Int, Int] = {
      case x if x % 2 != 0 => x + 1
    }

    val incrementOddAndThenEvenImplicit = incrementOddImplicit andThen incrementEvenImplicit

    assertEquals(incrementOddAndThenEvenImplicit(1), 3)

    val incrementOddOrEvenImplicit = incrementOddImplicit orElse incrementEvenImplicit

    assertEquals(incrementOddOrEvenImplicit(1), 2)
    assertEquals(incrementOddOrEvenImplicit(2), 3)

    assertEquals(List(1, 2, 3, 4).collect(incrementOddImplicit), List(2, 4))
    assertEquals(List(1, 2, 3, 4).collect(incrementEvenImplicit), List(3, 5))

    assertEquals(List(1, 3).map(incrementOddImplicit), List(2, 4))
    assertEquals(List(2, 4).map(incrementEvenImplicit), List(3, 5))

    assertEquals(List(1, 2, 3, 4).filter { x: Int => x % 2 != 0 }, List(1, 3))
    assertEquals(List(1, 2, 3, 4).filter { x: Int => x % 2 == 0 }, List(2, 4))
  }

  test("ADT") {
    sealed trait Color // sum type
    case object Red   extends Color
    case object Green extends Color
    case object Blue  extends Color

    // case class vs case object - single instance
    // object vs case object toString, improved pattern matching

    object Color {
      def values: Set[Color] = Set(Red, Green, Blue)

      implicit class ColorOps(in: Color) {
        def hex: String = in match {
          case Red   => "#F00"
          case Green => "#0F0"
          case Blue  => "#00F"
        }
      }
    }

    assertEquals(Red.toString, "Red")
    assertEquals(Color.values.map(_.hex), Set("#F00", "#0F0", "#00F"))

    sealed trait Error // hybrid type
    case class NotFound(message: String, code: Int = 404)   extends Error // product type
    case class BadRequest(message: String, code: Int = 400) extends Error
    case class InternalError(code: Int = 500)               extends Error
    case object Unknown                                     extends Error

    object Error {
      implicit class ErrorOps(in: Error) {
        def asString: String =
          in match {
            case NotFound(message, code)   => s"$code error: $message"
            case BadRequest(message, code) => s"$code error: $message"
            case InternalError(code)       => s"$code error: try again later"
            case Unknown                   => "Unknown error"
          }
      }
    }

    assertEquals(NotFound("Resource not found").asString, "404 error: Resource not found")
    assertEquals(BadRequest("Invalid form field: username").asString, "400 error: Invalid form field: username")
    assertEquals(InternalError().asString, "500 error: try again later")
    assertEquals(Unknown.asString, "Unknown error")
  }

  test("Collections - partition") {
    val (odd, even) = List(1, 2, 3, 4, 5, 6, 7).partition(_ % 2 != 0)

    assertEquals(odd, List(1, 3, 5, 7))
    assertEquals(even, List(2, 4, 6))
  }

  test("trait self") {
    trait CommonQueries {
      def getBy(id: String): String
      def getAll: String
    }

    trait ManagementQueries {
      this: CommonQueries =>
      def deleteBy(id: String): String
    }

    class SuperAdminPanel extends ManagementQueries with CommonQueries {
      override def getAll                       = "getAll records"
      override def getBy(id: String)            = s"getBy id: $id any record"
      override def deleteBy(id: String): String = s"deleteBy id: $id any record"
    }

    class AdminPanel extends ManagementQueries with CommonQueries {
      override def getAll                       = "getAll records"
      override def getBy(id: String)            = s"getBy id: $id any record"
      override def deleteBy(id: String): String = s"deleteBy id: $id record with access"
    }

    class UserPanel extends CommonQueries {
      override def getAll            = "getAll records of authorized user"
      override def getBy(id: String) = s"getBy id: $id of authorized user"
    }

    val superAdminPanel = new SuperAdminPanel
    val adminPanel      = new AdminPanel
    val userPanel       = new UserPanel

    assertEquals(userPanel.getAll, "getAll records of authorized user")
    assertEquals(userPanel.getBy("id"), "getBy id: id of authorized user")
    assertEquals(superAdminPanel.getAll, "getAll records")
    assertEquals(adminPanel.getAll, "getAll records")
    assertEquals(adminPanel.getBy("id"), "getBy id: id any record")
    assertEquals(adminPanel.deleteBy("id"), "deleteBy id: id record with access")
    assertEquals(superAdminPanel.deleteBy("id"), "deleteBy id: id any record")
  }
}
