package aoc2022

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day9.*

object Day9Suite extends ZIOSpecDefault:

  val example = List(
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
  )

  val example2 = List(
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
  )

  val exampleStream =
    ZStream.fromIterable(example)

  val example2Stream =
    ZStream.fromIterable(example2)

  lazy val spec =
    suite("day9")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(13))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(5710))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(1))
        },
        test("example2") {
          assertZIO(part2(example2Stream))(equalTo(36))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(2259))
        }
      )
    )
