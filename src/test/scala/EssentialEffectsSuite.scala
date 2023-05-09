package com.example

import cats.Parallel
import cats.effect.IO
import cats.implicits.{
  catsSyntaxParallelSequence1,
  catsSyntaxParallelTraverse1,
  catsSyntaxTuple2Parallel,
  catsSyntaxTuple2Semigroupal,
  catsSyntaxTuple3Parallel,
  toTraverseOps
}
import munit.CatsEffectSuite

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{ DurationInt, FiniteDuration }

class EssentialEffectsSuite extends CatsEffectSuite with EssentialEffectsSuiteContext {
  test("unsafeRun") {
    val helloWorld = for {
      _ <- MyIO.putStr("hello")
      _ <- MyIO.putStr("world")
    } yield ()

    helloWorld.unsafeRun()
  }

  test("Timing") {
    val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

    def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
      for {
        start  <- clock
        result <- action
        end    <- clock
      } yield (FiniteDuration(end - start, TimeUnit.MILLISECONDS), result)

    val helloWorld = for {
      _ <- MyIO.putStr("hello")
      _ <- MyIO(() => Thread.sleep(100L))
      _ <- MyIO.putStr("world")
    } yield ()

    val timedHello = time(helloWorld)

    timedHello.unsafeRun() match {
      case (duration, _) => println(s"hello took $duration")
    }
  }

  test("Ticking Clock".ignore) {
    def program: IO[Unit] = for {
      time <- IO(System.currentTimeMillis())
      _    <- IO.println(time)
      _    <- IO.sleep(1.second)
      _    <- program
    } yield ()

    program.unsafeRunSync()
  }

  test("Parallel") {
    val iap = Parallel[IO].parallel(ia)
    val iab = Parallel[IO].parallel(ib)

    val program1 = (iap, iab).mapN((a, b) => (a, b))

    assertIO(Parallel[IO].sequential(program1).print, (1, 2))
  }

  test("Parallel - parMapN") {
    val program = (ia, ib).parMapN((a, b) => (a, b))

    assertIO(program.print, (1, 2))
  }

  test("Parallel - parTupled") {
    val program = (ia, ib).parTupled

    assertIO(program, (1, 2))
  }

  test("Parallel - parTraverse") {
    val program1 = List(1, 2).parTraverse(a => IO(a + 1))
    val program2 = List(3, 4).map(a => IO(a + 1)).sequence
    val program3 = List(5, 6).map(a => IO(a + 1)).parSequence

    assertIO(program1, List(2, 3))
    assertIO(program2, List(4, 5))
    assertIO(program3, List(6, 7))
  }

  test("Parallel - parSequence") {
    val program1 = List(ia, ib).parSequence
    val program2 = List(ia, ib).parTraverse(identity)

    assertIO(program1, List(1, 2))
    assertIO(program2, List(1, 2))
  }

  test("Parallel - error fast fail") {
    val program1 = List(ia, IO.raiseError(new RuntimeException("error 1")), ib).parSequence
    val program2 = (ia, IO.raiseError(new RuntimeException("error 2")), ib).parTupled
    val program3 = (ia, IO.raiseError(new RuntimeException("error 3")), ib).parMapN((a, b: Int, c) => (a, b, c))
    val program4 = List(ia, ib).parTraverse(_ => IO.raiseError(new RuntimeException("error 4")))

    assertIO(program1.attempt.map(_.left.map(_.getMessage)), Left("error 1"))
    assertIO(program2.attempt.map(_.left.map(_.getMessage)), Left("error 2"))
    assertIO(program3.attempt.map(_.left.map(_.getMessage)), Left("error 3"))
    assertIO(program4.attempt.map(_.left.map(_.getMessage)), Left("error 4"))
  }
}

trait EssentialEffectsSuiteContext {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B]           = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  object MyIO {
    def putStr(s: => String): MyIO[Unit] = MyIO(() => println(s))
  }

  implicit class IODebugOps[A](ioa: IO[A]) {
    def print: IO[A] =
      for {
        a      <- ioa
        thread <- IO(Thread.currentThread().getName)
        _      <- IO.println(s"Thread: $thread, value: $a")
      } yield a
  }

  val ia = IO(1).print
  val ib = IO(2).print
}
