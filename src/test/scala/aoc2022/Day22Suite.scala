package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day22.*
import Day22.Orientation.*
import Day22.Rotation.*

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

  val exampleFaceMapPart1: Day22.FaceMap =
    Map(
      0 -> Map(Right -> (0, Rot0), Down -> (3, Rot0), Left -> (0, Rot0), Up -> (4, Rot0)),
      1 -> Map(Right -> (2, Rot0), Down -> (1, Rot0), Left -> (2, Rot0), Up -> (1, Rot0)),
      2 -> Map(Right -> (3, Rot0), Down -> (2, Rot0), Left -> (1, Rot0), Up -> (2, Rot0)),
      3 -> Map(Right -> (1, Rot0), Down -> (4, Rot0), Left -> (2, Rot0), Up -> (0, Rot0)),
      4 -> Map(Right -> (4, Rot0), Down -> (0, Rot0), Left -> (5, Rot0), Up -> (3, Rot0)),
      5 -> Map(Right -> (4, Rot0), Down -> (5, Rot0), Left -> (4, Rot0), Up -> (5, Rot0))
    )

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  val exampleSize = 4

  val exampleFaceMapPart2: Day22.FaceMap =
    Map(
      0 -> Map(Right -> (5, Rot180), Down -> (3, Rot0), Left -> (2, Rot270), Up -> (1, Rot180)),
      1 -> Map(Right -> (2, Rot0), Down -> (4, Rot180), Left -> (5, Rot90), Up -> (0, Rot180)),
      2 -> Map(Right -> (3, Rot0), Down -> (4, Rot270), Left -> (1, Rot0), Up -> (0, Rot90)),
      3 -> Map(Right -> (5, Rot90), Down -> (4, Rot0), Left -> (2, Rot0), Up -> (0, Rot0)),
      4 -> Map(Right -> (5, Rot0), Down -> (1, Rot180), Left -> (2, Rot90), Up -> (3, Rot0)),
      5 -> Map(Right -> (0, Rot180), Down -> (1, Rot270), Left -> (4, Rot0), Up -> (3, Rot270))
    )

  val spec =
    suite("day22")(
      suite("part1")(
        test("example") {
          assertZIO(part(exampleStream, exampleShape, exampleSize, exampleFaceMapPart1))(
            equalTo(6032)
          )
        },
        test("input.txt") {
          assertZIO(part(inputStream, Day22.inputShape, Day22.inputSize, Day22.inputFaceMapPart1))(
            equalTo(149250)
          )
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part(exampleStream, exampleShape, exampleSize, exampleFaceMapPart2))(
            equalTo(5031)
          )
        },
        test("input.txt") {
          assertZIO(part(inputStream, Day22.inputShape, Day22.inputSize, Day22.inputFaceMapPart1))(
            equalTo(0)
          )
        } @@ ignore
      )
    )
