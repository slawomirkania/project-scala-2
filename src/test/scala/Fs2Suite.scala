package com.example

import cats.effect.IO
import fs2.io.file.{ Files, Flags, Path }
import fs2.{ text, Pipe, Pure, Stream }
import munit.CatsEffectSuite

class Fs2Suite extends CatsEffectSuite {
  test("multiply numbers through the pipe") {
    def mulPure: Pipe[Pure, Int, Int] = _.map(_ * 2)

    val result = Stream
      .emits(List(1, 2))
      .through(mulPure)
      .toList

    assertEquals(result, List(2, 4))
  }

  test("lift stream values to IO") {
    def mulIO: Pipe[IO, Int, Int] = _.map(_ * 2)

    val result = Stream
      .emits(List(1, 2))
      .covary[IO]
      .through(mulIO)
      .compile
      .toList

    assertIO(result, List(2, 4))
  }

  test("unNone") {
    val result = Stream
      .emits(List(Some(1), None, Some(2)))
      .unNone
      .toList

    assertEquals(result, List(1, 2))
  }

  test("read numbers from file to Stream") {
    val input = Path(getClass.getResource("/input").getPath)

    val result = Files[IO]
      .readAll(input)
      .through(text.utf8.decode)
      .through(text.lines)
      .covary[IO]
      .map(_.toLong)
      .take(2)
      .compile
      .toList

    assertIO(result, List[Long](3, 4))
  }

  test("write numbers to file") {
    val output = Path("output")

    def toFile: Pipe[IO, String, Unit] =
      _.intersperse("\n")
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(output, Flags.Write))

    val result = Stream
      .emits(List(1, 2))
      .map(_.toString)
      .through(toFile)
      .compile
      .drain
      .flatMap { _ =>
        Files[IO]
          .readAll(output)
          .through(text.utf8.decode)
          .through(text.lines)
          .covary[IO]
          .map(_.toLong)
          .take(10)
          .compile
          .toList
      }

    assertIO(result, List[Long](1, 2))
  }

  test("evalTap") {
    val result = Stream
      .emits(List(1, 2))
      .covary[IO]
      .evalTap(IO.println)
      .compile
      .toList

    assertIO(result, List(1, 2))
  }

  test("parEvalTap") {
    val result = Stream
      .emits(List(1, 2))
      .covary[IO]
      .parEvalMap(4)(i => IO.println("incrementing: " + i) >> IO(i + 1))
      .compile
      .toList

    assertIO(result, List(2, 3))
  }
}
