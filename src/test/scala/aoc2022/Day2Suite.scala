package aoc2022

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
      suite("part1")(
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(15))
        }
      ),
      suite("part2")(
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(12))
        }
      )
    )
