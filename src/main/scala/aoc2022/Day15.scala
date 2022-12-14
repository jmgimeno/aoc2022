package aoc2022

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

  case class Range(begin: Int, endIncluded: Int)

  case class Reading(sensor: Position, beacon: Position):
    val radius = sensor manhattan beacon
    val minX = sensor.x - radius
    val maxX = sensor.x + radius
    def outerPerimeter =
      sensor.circumference(radius + 1)
    def includes(position: Position) =
      sensor.manhattan(position) <= radius

  object Reading:
    def parse(line: String) = line match
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        Reading(Position(sx.toInt, sy.toInt), Position(bx.toInt, by.toInt))

  case class Report(readings: List[Reading]):
    val minX = readings.map(_.minX).min
    val maxX = readings.map(_.maxX).max

    def notPresentAt(row: Int) =
      val covered = (minX to maxX)
        .map { x => Position(x, row) }
        .count { position =>
          readings.exists { reading =>
            reading.includes(position)
          }
        }
      val beacons =
        readings.filter(_.beacon.y == row).map(_.beacon.x).toSet.size
      covered - beacons

    extension [A, B](list: List[A])
      def findSome(p: A => Option[B]): Option[B] = list match
        case head :: tail => p(head).orElse(tail.findSome(p))
        case Nil          => None

    def findUncovered(limit: Int) =
      // if it's only one possible point => it's in the outer rim of a reading
      readings
        .findSome { currentReading =>
          currentReading.outerPerimeter
            .filter(_.inBounds(limit))
            .find { position =>
              readings
                .filterNot(_ == currentReading) // it's outside currentReading
                .forall { otherReading =>
                  !otherReading.includes(position)
                }
            }
        }
        .getOrElse(assert(false, "should't happend"))

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

  def part2(bound: Int)(is: UStream[String]): Task[Long] =
    for report <- is
        .map(Reading.parse)
        .runCollect
        .map(readings => Report(readings.toList))
    yield report
      .findUncovered(bound)
      .pipe(pos => 4_000_000L * pos.x + pos.y)

  lazy val run =
    part1(2_000_000)(inputStream).debug("PART1")
      *> part2(4_000_000)(inputStream).debug("PART2")
