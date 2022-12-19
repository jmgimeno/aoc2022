package aoc2022

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day10.*

object Day10Suite extends ZIOSpecDefault:

  val exampleStream =
    ZStream
      .fromFileName("data/example10.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  val examplePart2 =
    """##..##..##..##..##..##..##..##..##..##..
      |###...###...###...###...###...###...###.
      |####....####....####....####....####....
      |#####.....#####.....#####.....#####.....
      |######......######......######......####
      |#######.......#######.......#######.....""".stripMargin

  val inputPart2 =
    """###...##..#....###..###..####..##..#..#.
      |#..#.#..#.#....#..#.#..#....#.#..#.#..#.
      |#..#.#....#....#..#.###....#..#..#.#..#.
      |###..#.##.#....###..#..#..#...####.#..#.
      |#.#..#..#.#....#.#..#..#.#....#..#.#..#.
      |#..#..###.####.#..#.###..####.#..#..##..""".stripMargin

  lazy val spec =
    suite("day10")(
      suite("part1")(
        test("example.txt") {
          assertZIO(part1(exampleStream))(equalTo(13140))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(14420))
        }
      ),
      suite("part2")(
        test("example.txt") {
          assertZIO(part2(exampleStream))(equalTo(examplePart2))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(inputPart2))
        }
      )
    )
