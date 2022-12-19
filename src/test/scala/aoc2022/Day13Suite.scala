package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day13.*

object Day13Suite extends ZIOSpecDefault:

  val example =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day12")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(13))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(6428))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(140))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(22464))
        }
      )
    )
