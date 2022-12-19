package aoc2022

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day4.*

object Day4Suite extends ZIOSpecDefault:

  val example = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  val exampleStream = ZStream.fromIterable(example)

  def spec =
    suite("day4")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(2))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(444))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(4))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(801))
        }
      )
    )
