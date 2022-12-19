package aoc2022

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day8.*

object Day8Suite extends ZIOSpecDefault:

  val example = List(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  )

  val exampleStream =
    ZStream.fromIterable(example)

  lazy val spec =
    suite("day8")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(21))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(1711))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(8))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(301392))
        }
      )
    )
