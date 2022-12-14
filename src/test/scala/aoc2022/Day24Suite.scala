package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day24.*

object Day24Suite extends ZIOSpecDefault:

  val example =
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  val spec =
    suite("day24")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(18))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(281))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(54))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(807))
        }
      )
    )
