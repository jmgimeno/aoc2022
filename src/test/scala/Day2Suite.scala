import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day2.*

object Day2Suite extends ZIOSpecDefault:

  val input = List(
    "A Y",
    "B X",
    "C Z"
  )

  val inputStream = ZStream.fromIterable(input)

  val spec =
    suite("day2")(
      test("part1") {
        assertZIO(part1(inputStream))(equalTo(15))
      },
      test("part2") {
        assertZIO(part2(inputStream))(equalTo(12))
      }
    )
