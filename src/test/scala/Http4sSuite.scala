package com.example

import cats.effect.IO
import org.http4s.client.Client
import io.circe.{ Decoder, HCursor }
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.circe.jsonOf
import org.http4s.client.middleware.{ Retry, RetryPolicy }
import org.http4s.dsl.io._
import org.http4s.implicits._

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class Http4sSuite extends CatsEffectSuite {
  import Http4sSuite._

  test("Response decoding") {
    val result: IO[Response[IO]] = routes.orNotFound.run(validRequest)

    assertIO(result.flatMap(_.as[FooBar]), FooBar("f", "b"))
  }

  test("404") {
    val result: IO[Response[IO]] = routes.orNotFound.run(invalidRequest)

    assertIO(result.map(_.status), NotFound)
  }

  test("client") {
    val client: Client[IO] = Client.fromHttpApp(routes.orNotFound)

    assertIO(client.expect[FooBar](validRequest), FooBar("f", "b"))
  }

  test("middleware - valid request") {
    val result = Middleware(routes).orNotFound.run(validRequest)

    assertIO(
      result.map(_.headers.headers.map(_.value)),
      List("text/plain; charset=UTF-8", "24", "someValue")
    )
  }

  test("middleware - invalid request") {
    val result = Middleware(routes).orNotFound.run(invalidRequest)

    assertIO(
      result.map(_.headers.headers.map(_.value)),
      List("text/plain; charset=UTF-8", "9")
    )
  }

  test("Retry middleware - exponentialBackoff") {
    val client: Client[IO] =
      Retry[IO](RetryPolicy(RetryPolicy.exponentialBackoff(2.seconds, 6)))(Client.fromHttpApp(routes.orNotFound))

    assertIO(client.expect[FooBar](retryErrorsRequest), FooBar("f", "b"))
  }
}

object Http4sSuite {
  case class FooBar(foo: String, bar: String)
  object FooBar {
    implicit val nameDecoder: Decoder[FooBar] = (c: HCursor) =>
      for {
        foo <- c.downField("foo").as[String]
        bar <- c.downField("bar").as[String]
      } yield FooBar(foo, bar)

    implicit val nameEntityDecoder = jsonOf[IO, FooBar]
  }

  val retryErrorsResponsesQueue = new mutable.Queue[IO[Response[IO]]]()
    .enqueue(
      RequestTimeout("Timeout"),
      InternalServerError("Internal server error"),
      ServiceUnavailable("Unavailable"),
      BadGateway("Bad gateway"),
      GatewayTimeout("Gateway timeout"),
      Ok(s"""{"foo": "f", "bar": "b"}""")
    )

  val routes = HttpRoutes
    .of[IO] {
      case GET -> Root / "hello" / foo / bar      => Ok(s"""{"foo": "$foo", "bar": "$bar"}""")
      case GET -> Root / "hello" / "retry-errors" => retryErrorsResponsesQueue.dequeue()
    }

  val validRequest       = Request[IO](Method.GET, uri"/hello/f/b")
  val invalidRequest     = Request[IO](Method.GET, uri"/invalid")
  val retryErrorsRequest = Request[IO](Method.GET, uri"/hello/retry-errors")

  object Middleware {
    def addHeader(response: Response[IO])(headers: Header.ToRaw): Response[IO] =
      response.putHeaders(headers)

    def apply(routes: HttpRoutes[IO]): HttpRoutes[IO] =
      routes.map(addHeader(_)("someKey" -> "someValue"))
  }
}
