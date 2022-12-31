package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day20.*

object Day20Suite extends ZIOSpecDefault:

  lazy val example =
    """1
      |2
      |-3
      |3
      |-2
      |0
      |4""".stripMargin

  lazy val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day20")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(3))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(2203))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(0))
        } @@ ignore,
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(0))
        } @@ ignore
      )
    )
