import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day9.*

object Day9Suite extends ZIOSpecDefault:

  val example = List(
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
  )

  val exampleStream =
    ZStream.fromIterable(example)

  lazy val spec =
    suite("day9")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(13))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(5710))
        }
      ),
      suite("part2")(
        test("example") {
          assertTrue(true)
        },
        test("input.txt") {
          assertTrue(true)
        }
      )
    )
