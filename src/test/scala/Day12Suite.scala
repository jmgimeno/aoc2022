import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*

import Day12.*

object Day12Suite extends ZIOSpecDefault:

  val example =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day12")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(31))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(408))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(29))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(399))
        }
      )
    )
