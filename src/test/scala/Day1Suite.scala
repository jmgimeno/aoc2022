import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day1.*

object Day1Suite extends ZIOSpecDefault:

  val input = List(
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
  )

  val inputStream =
    ZStream.fromIterable(input)

  val spec = suite("day1")(
    suite("part1")(
      test("example") {
        assertTrue(part1(input) == 24000)
      },
      test("input.txt") {
        assertZIO(part1Workflow(inputStream))(equalTo(24000))
      }
    ),
    suite("part2")(
      test("example") {
        assertTrue(part2(input) == 45000)
      },
      test("input.txt") {
        assertZIO(part2Workflow(inputStream))(equalTo(45000))
      }
    )
  )
