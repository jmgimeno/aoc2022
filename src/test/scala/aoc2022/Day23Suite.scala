package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day23.*

object Day23Suite extends ZIOSpecDefault:

  val example =
    """....#..
      |..###.#
      |#...#.#
      |.#...##
      |#.###..
      |##.#.##
      |.#..#..""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  val spec =
    suite("day23")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(110))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(4146))
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
