package aoc2022

import zio.*
import zio.stream.*

object Day4 extends ZIOAppDefault:

  case class Interval(lower: Int, upper: Int):
    def fullyContains(other: Interval): Boolean =
      lower <= other.lower && other.upper <= upper
    def intersects(other: Interval): Boolean =
      math.max(lower, other.lower) <= math.min(upper, other.upper)

  object Interval:
    def parse(input: String): Interval =
      val parts = input.split("-")
      Interval(parts(0).toInt, parts(1).toInt)

  case class Pair(interval1: Interval, interval2: Interval):
    def hasFullContained: Boolean =
      interval1.fullyContains(interval2) || interval2.fullyContains(interval1)
    def hasIntersection: Boolean =
      interval1.intersects(interval2) || interval2.intersects(interval1)

  object Pair:
    def parse(input: String): Pair =
      val parts = input.split(",")
      Pair(Interval.parse(parts(0)), Interval.parse(parts(1)))

  val inputStream =
    ZStream
      .fromFileName("data/input4.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)

  def part1[R, E](inputStream: ZStream[R, E, String]): ZIO[R, E, Long] =
    inputStream
      .map(Pair.parse)
      .filter(_.hasFullContained)
      .runCount

  def part2[R, E](inputStream: ZStream[R, E, String]): ZIO[R, E, Long] =
    inputStream
      .map(Pair.parse)
      .filter(_.hasIntersection)
      .runCount

  val run = part1(inputStream).debug *> part2(inputStream).debug
