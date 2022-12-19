package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day14.*

object Day14Suite extends ZIOSpecDefault:

  lazy val example =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  lazy val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day14")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(24))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(885))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(93))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(28691))
        }
      )
    )
