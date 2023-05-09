package com.example

import cats.effect.IO
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
}

trait EssentialEffectsSuiteContext {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B]           = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  object MyIO {
    def putStr(s: => String): MyIO[Unit] = MyIO(() => println(s))
  }
}
