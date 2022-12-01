import zio.*
import zio.test.*

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

  val spec = suite("day1")(
    test("part1") {
      assertTrue(part1(input) == 24000)
    },
    test("part2") {
      assertTrue(part2(input) == 45000)
    }
  )
