import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day15 extends ZIOAppDefault:

  case class Position(x: Int, y: Int):
    def manhattan(other: Position) =
      math.abs(x - other.x) + math.abs(y - other.y)
    def circumference(radius: Int) =
      (for
        d <- 0 to radius
        px <- List(x - d, x + d)
        py <- List(y - radius + d, y + radius - d)
      yield Position(px, py)).toSet
    def inBounds(limit: Int) =
      0 <= x && x <= limit && 0 <= y && y <= limit
    def part2 = 4_000_000 * x + y

  case class Range(begin: Int, endIncluded: Int):
    def toSet = (begin to endIncluded).toSet

  case class Reading(sensor: Position, beacon: Position):
    val radius = sensor manhattan beacon
    def notPresentAt(y: Int) =
      val diff = radius - math.abs(y - sensor.y)
      if diff < 0 then None
      else Some(Range(sensor.x - diff, sensor.x + diff))
    def outerPerimeter =
      sensor.circumference(radius + 1)

  object Reading:
    def parse(line: String) = line match
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        Reading(Position(sx.toInt, sy.toInt), Position(bx.toInt, by.toInt))

  case class Report(readings: List[Reading]):

    private def coveredAt2(row: Int) =
      val array = readings
        .map(_.notPresentAt(row))
        .collect { case Some(range) =>
          range.toSet
        }
        .toArray
      foldTree(array, 0, array.length)

    private def foldTree(
        array: Array[Set[Int]],
        begin: Int,
        end: Int
    ): Set[Int] =
      if begin == end then Set.empty
      else if begin == end - 1 then array(begin)
      else
        var mid = (begin + end) / 2
        foldTree(array, begin, mid) union foldTree(array, mid, end)

    private def coveredAt(row: Int) =
      readings
        .map(_.notPresentAt(row))
        .collect { case Some(range) =>
          range.toSet
        }
        .foldLeft(Set.empty[Int])(_ union _)

    private def beaconsAt(row: Int) =
      readings.filter(_.beacon.y == row).map(_.beacon.x).toSet

    def notPresentAt(row: Int) =
      (coveredAt(row) diff beaconsAt(row)).size

    def notPresentAt2(row: Int) =
      (coveredAt2(row) diff beaconsAt(row)).size

    def notPresentAt3(row: Int) =
      // most efficient one
      val included = mutable.Set[Int]()
      readings.foreach { reading =>
        reading.notPresentAt(row).foreach { range =>
          included.addAll(range.begin to range.endIncluded)
        }
      }
      readings
        .filter(_.beacon.y == row)
        .map(_.beacon.x)
        .foreach(included.remove)
      included.size

    def findUncovered(limit: Int): Int =
      ???

  lazy val inputStream =
    ZStream
      .fromFileName("data/input15.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(row: Int)(is: UStream[String]): Task[Int] =
    for report <- is
        .map(Reading.parse)
        .runCollect
        .map(readings => Report(readings.toList))
    yield report.notPresentAt3(row)

  def part2(bound: Int)(is: UStream[String]): Task[Int] =
    for report <- is
        .map(Reading.parse)
        .runCollect
        .map(readings => Report(readings.toList))
    yield report.findUncovered(bound)

  lazy val run =
    part1(2_000_000)(inputStream).debug("PART1")
      *> part2(4_000_000)(inputStream).debug("PART2")
