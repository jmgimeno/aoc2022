package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import java.io.File

object Day8 extends ZIOAppDefault:

  enum Direction:
    case UP, RIGHT, DOWN, LEFT

  case class Forest private (trees: Vector[Vector[Int]]):
    val height = trees.size
    val width = if trees.isEmpty then 0 else trees(0).size
    val edge = 2 * width + 2 * (height - 2)

    def addRow(row: Vector[Int]): Forest =
      Forest {
        trees.appended(row)
      }

    def visibleFromOutside: Int =
      edge + visibleFromInside

    def visibleFromInside: Int =
      val innerVisibilities =
        for
          y <- 1 until width - 1
          x <- 1 until height - 1
        yield isVisible(x, y)
      innerVisibilities.count(identity)

    def isVisible(x: Int, y: Int): Boolean =
      Direction.values.exists(isVisible(x, y))

    def isVisible(x: Int, y: Int)(d: Direction): Boolean =
      lineOfSight(x, y)(d).forall(_ < trees(y)(x))

    def lineOfSight(x: Int, y: Int)(d: Direction): IndexedSeq[Int] =
      d match
        case Direction.UP    => (y - 1 to 0 by -1).map(trees(_)(x))
        case Direction.LEFT  => (x - 1 to 0 by -1).map(trees(y)(_))
        case Direction.DOWN  => (y + 1 until height).map(trees(_)(x))
        case Direction.RIGHT => (x + 1 until width).map(trees(y)(_))

    def highestScenicScore: Int =
      val scenicScores =
        for
          x <- 1 until width - 1
          y <- 1 until height - 1
        yield scenicScore(x, y)
      scenicScores.max

    def scenicScore(x: Int, y: Int): Int =
      Direction.values
        .map(viewingDistance(x, y))
        .foldLeft(1)(_ * _)

    def viewingDistance(x: Int, y: Int)(d: Direction): Int =
      viewingDistance(lineOfSight(x, y)(d), trees(y)(x))

    def viewingDistance(lineOfSight: IndexedSeq[Int], tree: Int): Int =
      val pos = lineOfSight.indexWhere(_ >= tree)
      if pos == -1 then lineOfSight.length else pos + 1

  object Forest:
    val empty: Forest = new Forest(Vector.empty)

  val inputStream =
    ZStream
      .fromFileName("data/input8.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  def part1[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    for forest <-
        is.runFold(Forest.empty) { (f, l) =>
          f.addRow(l.map(_.toString.toInt).toVector)
        }
    yield forest.visibleFromOutside

  def part2[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    for forest <-
        is.runFold(Forest.empty) { (f, l) =>
          f.addRow(l.map(_.toString.toInt).toVector)
        }
    yield forest.highestScenicScore

  val run =
    part1(inputStream).debug("part1") *> part2(inputStream).debug("part2")
