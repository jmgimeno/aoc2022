package aoc2022

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day5.*

object Day5Suite extends ZIOSpecDefault:

  val example = List(
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  )

  val exampleStream = ZStream.fromIterable(example)

  lazy val spec =
    suite("day5")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo("CMZ"))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo("JDTMRWCQJ"))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo("MCD"))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo("VHJDDCWRD"))
        }
      )
    )
