package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day17.*

object Day17Suite extends ZIOSpecDefault:

  val example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

  val exampleStream = ZStream.fromIterable(example)

  val spec =
    suite("day17")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(3068))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(3102))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(1514285714288L))
        } @@ ignore,
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(0))
        } @@ ignore
      )
    )
