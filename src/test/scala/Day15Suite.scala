import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day15.*

object Day15Suite extends ZIOSpecDefault:

  lazy val example =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  lazy val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day15")(
      suite("part1")(
        test("example") {
          assertZIO(part1(10)(exampleStream))(equalTo(26))
        },
        test("input.txt") {
          assertZIO(part1(2_000_000)(inputStream))(equalTo(5832528))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(20)(exampleStream))(equalTo(56000011))
        },
        test("input.txt") {
          assertZIO(part2(4_000_000)(inputStream))(equalTo(13360899249595L))
        } @@ ignore
      )
    )
