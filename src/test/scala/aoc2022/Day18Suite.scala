package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day18.*

object Day18Suite extends ZIOSpecDefault:

  val example =
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  val spec =
    suite("day18")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(64))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(3542))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(58))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(2080))
        }
      )
    )
