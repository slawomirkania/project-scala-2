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
}
