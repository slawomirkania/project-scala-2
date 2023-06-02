package com.example

import cats.Parallel
import cats.effect.IO
import cats.implicits._
import munit.CatsEffectSuite
import cats.data.EitherNes
import cats.effect.kernel.Resource
import cats.effect.testkit.TestControl

import java.io.RandomAccessFile
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.util.{ Failure, Success }
import scala.util.control.NonFatal

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

  test("parMapN vs mapN vs flatMapN - EitherNes") {
    final case class Error(value: String)
    final case class Part(value: String)
    final case class Full(part1: Part, part2: Part, part3: Part)

    implicit val orderError: cats.Order[Error] = cats.Order.by(_.value)

    val ok: EitherNes[Error, Part]     = Part("ok").asRight.toEitherNes
    val error1: EitherNes[Error, Part] = Error("error 1").asLeft[Part].toEitherNes
    val error2: EitherNes[Error, Part] = Error("error 2").asLeft[Part].toEitherNes

    assertEquals((error1, ok, error2).mapN(Full.apply).leftMap(_.toList), List(Error("error 1")).asLeft[Full])
    assertEquals(
      (error1, ok, error2).flatMapN(Full.apply(_, _, _).asRight[Error].toEitherNes).leftMap(_.toList),
      List(Error("error 1")).asLeft[Full]
    )
    assertEquals(
      (error1, ok, error2).parMapN(Full.apply).leftMap(_.toList),
      List(Error("error 1"), Error("error 2")).asLeft[Full]
    )
  }

  test("error handling") {
    assertIO(IO.raiseError(new RuntimeException("boom")).handleError(_ => "fallback"), "fallback")

    assertIO(
      IO.raiseError(new RuntimeException("boom")).recover { case NonFatal(_) =>
        "fallback"
      },
      "fallback"
    )
    assertIO(IO.raiseError(new RuntimeException("boom")).handleErrorWith(_ => IO("fallback")), "fallback")

    assertIO(
      IO.raiseError(new RuntimeException("boom")).recoverWith { case NonFatal(_) =>
        IO("fallback")
      },
      "fallback"
    )

    assertIO(
      IO(false).ensure(new RuntimeException("boom"))(_ == true).attempt.map(_.leftMap(_.getMessage)),
      Left("boom")
    )

    assertIO(
      IO(false)
        .ensureOr(s => new RuntimeException(s"value=$s, error: boom"))(_ == true)
        .attempt
        .map(_.leftMap(_.getMessage)),
      Left("value=false, error: boom")
    )

    assertIO(
      IO.raiseError(new RuntimeException("boom"))
        .adaptError { case NonFatal(_) =>
          new RuntimeException("new error")
        }
        .attempt
        .map(_.leftMap(_.getMessage)),
      Left("new error")
    )

    assertIO(
      IO("two")
        .reject {
          case "one" => new RuntimeException("error 1")
          case "two" => new RuntimeException("error 2")
          case _     => new RuntimeException("error any")
        }
        .attempt
        .map(_.leftMap(_.getMessage)),
      Left("error 2")
    )

    assertIO(
      IO.raiseError(new RuntimeException("boom"))
        .redeemWith(e => IO(s"${e.getMessage} recover"), _ => IO("bind")),
      "boom recover"
    )

    assertIO(
      IO(true).redeemWith(_ => IO("recover"), b => IO(s"$b bind")),
      "true bind"
    )

    assertIO(
      (for {
        _ <- IO("1").print
        _ <- IO("2").print
        _ <- IO.raiseError[String](new RuntimeException("boom")).print
        _ <- IO("3").print
      } yield ()).attempt.map(_.leftMap(_.getMessage)),
      Left("boom")
    )
  }

  test("IO.race") {
    val task    = (IO.sleep(30.minutes) >> IO("task")).onCancel(IO.println("task cancelled"))
    val timeout = (IO.sleep(10.minutes) >> IO("timeout")).onCancel(IO.println("timeout cancelled"))

    val program = IO.race(task, timeout)

    TestControl.executeEmbed(program).assertEquals("timeout".asRight)
  }

  test("Resource") {
    val result =
      Resource.make(IO.println("acquired") >> IO("one"))(_ => IO.println("closed")).map(_ + " two").use { s =>
        IO.println("processing...") >> IO(s + " three")
      }

    assertIO(result, "one two three")
  }

  test("Resource - release even after failure") {
    val result =
      Resource.make(IO.println("acquired") >> IO("one"))(_ => IO.println("closed")).map(_ + " two").use { _ =>
        IO.raiseError(new RuntimeException("Boom"))
      }

    assertIO(result.attempt.map(_.leftMap(_.getMessage)), Left("Boom"))
  }

  test("Resource - read and close file") {
    val path = getClass.getResource("/input2").getPath

    trait FileReader {
      def read(offset: Int, length: Int): IO[(String, Int)]
      def close: IO[Unit]
    }

    object FileReader {
      def make(path: String): IO[FileReader] = IO {
        new FileReader {
          private val file = new RandomAccessFile(path, "r")

          def read(offset: Int, length: Int) = {
            file.seek(offset)
            val buf = new Array[Byte](length)
            val len = file.read(buf)
            IO((buf.map(_.toChar).mkString, offset + len))
          }

          def close = IO(file.close())
        }
      }
    }

    def read(offset: Int, length: Int) = Resource
      .make(IO.println(s"Opening file $path") >> FileReader.make(path))(r =>
        IO.println(s"Closing file $path") >> IO(r.close)
      )
      .use(_.read(offset, length))

    val program = (
      read(0, 5),
      read(5, 4),
      read(9, 8)
    ).parTupled

    assertIO(program, (("some ", 5), ("text", 9), (" example", 17)))
  }

  test("Resource - Fiber - background task".ignore) {
    def loop(name: String) = (for {
      _ <- IO.sleep(200.milli)
      _ <- IO.println(s"task $name trigger")
    } yield ()).foreverM

    def backgroundTask(name: String) = Resource
      .make(IO.println(s"start $name") >> loop(name).start)(IO.println(s"cancel $name") >> _.cancel)
      .void

    backgroundTask("run").use { _ =>
      IO.println("Other task while background task is running") >> IO.sleep(5.seconds) >> IO.println(
        "Other task is done"
      )
    }
  }

  test("IO.background".ignore) {
    def loop(name: String) = (for {
      _ <- IO.sleep(200.milli)
      _ <- IO.println(s"task $name trigger")
    } yield ()).foreverM.onCancel(IO.println(s"cancel $name"))

    IO.println("start run") >> loop("run").background.use { _ =>
      IO.println("Other task while background task is running") >> IO.sleep(5.seconds) >> IO.println(
        "Other task is done"
      )
    }
  }

  test("Supervisor") {
    import cats.effect.std.Supervisor

    Supervisor[IO](await = true).use { supervisor =>
      val program = for {
        fib1 <- (IO.sleep(10.seconds) >> IO.println("first finished")).onCancel(IO.println("first canceled")).start
        fib2 <- (IO.sleep(15.seconds) >> IO.println("seconds finished")).onCancel(IO.println("second canceled")).start
        _    <- fib1.join
        _    <- fib2.join // leaky
      } yield ()

      supervisor.supervise(program.onCancel(IO.println("program canceled")).timeout(20.seconds)).void
    }
  }

  test("Resource early release") {
    val source = Resource.make(IO.println("reading source") >> IO.pure("source"))(_ => IO.println("closing source"))
    def config(source: String) =
      Resource.make(IO.println(s"making config from $source") >> IO.pure("cnf"))(_ => IO.println("closing config"))
    def conn(config: Config) = Resource.make(IO.println(s"opening DB connection with config $config"))(_ =>
      IO.println(s"closing DB connection with config $config")
    )

    final case class Config(value: String)

    (for {
      conf <- Resource.liftK(source.use(s => config(s).use(configValue => IO.pure(Config(configValue)))))
      _    <- conn(conf)
    } yield ()).use { _ =>
      IO.println("querying DB")
    }
  }

  test("Parallelism depends on CPUs") {
    def task(i: Int): IO[String] = IO(s"processing $i").print

    for {
      cpus <- IO(Runtime.getRuntime.availableProcessors())
      _    <- IO.println(s"available CPUs $cpus")
      _    <- List.range(0, cpus * 2).parTraverse(task)
    } yield ()
  }

  test("Context shifting".ignore) {
    for {
      _ <- IO("task 1").print
      _ <- IO("task 2").print
      _ <- IO("task 3").print
      _ <- IO("task 4").print
    } yield ()
  }

  test("Context shifting 2".ignore) {
    for {
      _ <- IO.blocking("task 1").print
      _ <- IO.blocking("task 2").print
      _ <- IO.blocking("task 3").print
      _ <- IO.blocking("task 4").print
    } yield ()
  }

  test("Context shifting 3".ignore) {
    (
      IO("non blocking 1").print,
      IO.blocking("blocking 1").print,
      IO("non blocking 2").print,
      IO.blocking("blocking 2").print
    ).parTupled
  }

  test("IO.async") {
    def fromFuture_(fio: IO[Future[Int]]): IO[Int] =
      fio.flatMap { cf =>
        IO.async_[Int] { cb =>
          cf.onComplete {
            case Success(a) => cb(Right(a))
            case Failure(e) => cb(Left(e))
          }
        }.flatTap(IO.println)
      }

    fromFuture_(IO(Future(1))) // should not start at all

    val one = fromFuture_(IO(Future(1))) // should not start yet

    assertIO(
      for {
        one <- one
        two <- fromFuture_(IO(Future(2)))
      } yield (one, two),
      (1, 2)
    )
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
