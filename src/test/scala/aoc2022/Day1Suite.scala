package aoc2022

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day1.*

object Day1Suite extends ZIOSpecDefault:

  val example = List(
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

  val exampleStream =
    ZStream.fromIterable(example)

  val spec =
    suite("day1")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(24000))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(68802))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(45000))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(205370))
        }
      )
    )
