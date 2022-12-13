import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day.*

object DaySuite extends ZIOSpecDefault:

  lazy val example =
    """
    """.stripMargin

  lazy val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(0))
        } @@ ignore,
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(0))
        } @@ ignore
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(0))
        } @@ ignore,
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(0))
        } @@ ignore
      )
    )
