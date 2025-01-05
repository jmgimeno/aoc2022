package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day22.*
import Day22.Orientation.*

object Day22Suite extends ZIOSpecDefault:

  val example =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5""".stripMargin

  val exampleShape =
    """  0
      |123
      |  45""".stripMargin

  val exampleFaceMap: Map[Int, Map[Orientation, Int]] =
    Map(
      0 -> Map(Right -> 0, Down -> 3, Left -> 0, Up -> 4),
      1 -> Map(Right -> 2, Down -> 1, Left -> 2, Up -> 1),
      2 -> Map(Right -> 3, Down -> 2, Left -> 1, Up -> 2),
      3 -> Map(Right -> 1, Down -> 4, Left -> 2, Up -> 0),
      4 -> Map(Right -> 4, Down -> 0, Left -> 5, Up -> 3),
      5 -> Map(Right -> 4, Down -> 5, Left -> 4, Up -> 5)
    )

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  val exampleSize = 4

  val spec =
    suite("day22")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream, exampleShape, exampleSize, exampleFaceMap))(equalTo(6032))
        },
        test("input.txt") {
          assertZIO(part1(inputStream, Day22.inputShape, Day22.inputSize, Day22.inputFaceMap))(
            equalTo(149250)
          )
        } @@ ignore
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
