import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day15 extends ZIOAppDefault:

  case class Position(x: Int, y: Int):
    def manhattan(other: Position) =
      math.abs(x - other.x) + math.abs(y - other.y)

  case class Range(begin: Int, endIncluded: Int):
    def toSet = (begin to endIncluded).toSet

  case class Reading(sensor: Position, beacon: Position):
    val radius = sensor manhattan beacon
    def notPresentAt(y: Int) =
      val diff = radius - math.abs(y - sensor.y)
      if diff < 0 then None
      else Some(Range(sensor.x - diff, sensor.x + diff))

  object Reading:
    def parse(line: String) = line match
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        Reading(Position(sx.toInt, sy.toInt), Position(bx.toInt, by.toInt))

  case class Report(readings: List[Reading]):
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
    yield report.notPresentAt(row)

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(2_000_000)(inputStream).debug("PART1") *> part2(inputStream).debug(
      "PART2"
    )
