package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec
import scala.annotation.newMain

object Day24 extends ZIOAppDefault:

  extension (n: Int)
    infix def %%(m: Int) =
      java.lang.Math.floorMod(n, m)

  final case class Position(x: Int, y: Int):
    def manhattan(other: Position) =
      math.abs(x - other.x) + math.abs(y - other.y)
    infix def +(direction: Direction) =
      Position(x + direction.dx, y + direction.dy)

  @tailrec private def mcd(a: Int, b: Int): Int =
    if a % b == 0 then b
    else mcd(b, a % b)

  private def mcm(a: Int, b: Int) = (a * b) / mcd(a, b)

  enum Direction(val dx: Int, val dy: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, +1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(+1, 0)

  enum Blizzard:
    case Horizontal(x0: Int, y: Int, dx: Int, width: Int)
    case Vertical(x: Int, y0: Int, dy: Int, height: Int)
    def apply(t: Int): Position = this match
      case Blizzard.Horizontal(x0, y, dx, width) => Position((x0 + dx * t) %% width, y)
      case Blizzard.Vertical(x, y0, dy, height)  => Position(x, (y0 + dy * t) %% height)

  case class Bounds(width: Int, height: Int):
    def inside(position: Position) =
      0 <= position.x && position.x < width && 0 <= position.y && position.y < height
    def neighbours(position: Position) =
      position ::
        Direction.values
          .map(position + _)
          .filter(inside)
          .toList

  case class Valley(width: Int, height: Int, blizzards: List[Blizzard]):

    // at least both example an input have this start/end
    val start = Position(0, -1)
    val end = Position(width - 1, height)

    val horizontal = blizzards
      .collect { case h: Blizzard.Horizontal =>
        h
      }
      .groupBy(_.y)

    val vertical = blizzards
      .collect { case v: Blizzard.Vertical =>
        v
      }
      .groupBy(_.x)

    def free(t: Int)(position: Position): Boolean =
      horizontal.getOrElse(position.y, List.empty).forall(_(t) != position)
        && vertical.getOrElse(position.x, List.empty).forall(_(t) != position)

    val bounds = Bounds(width, height)
    val period = mcm(width, height)

    def minPath: Int =
      val open = mutable.Queue(start -> 0)
      val cache = mutable.Map(0 -> blizzards.map(_(0)).toSet)
      val seen = mutable.Set(start -> 0)

      @tailrec def loop: Int =
        if open.isEmpty then Integer.MAX_VALUE
        else
          val (current, time) = open.dequeue()
          if end.manhattan(current) == 1 then time + 1
          else
            bounds
              .neighbours(current)
              .filterNot(
                cache.getOrElseUpdate((time + 1) % period, blizzards.map(_(time + 1)).toSet)
              )
              .foreach { position =>
                val newState = position -> (time + 1)
                if !seen(newState) then
                  open += newState
                  seen += newState
              }
            loop
      loop

  object Parser:
    def parseBlizzard(x: Int, y: Int, width: Int, height: Int, c: Char) = c match
      case '^' => Blizzard.Vertical(x, y, -1, height)
      case 'v' => Blizzard.Vertical(x, y, +1, height)
      case '<' => Blizzard.Horizontal(x, y, -1, width)
      case '>' => Blizzard.Horizontal(x, y, +1, width)
      case ccc => assert(false, s"$ccc shouldn't happen here")

    def parseInput(lines: Chunk[String]): Valley =
      val width = lines(0).length - 2
      val height = lines.size - 2
      val blizzards = for
        x <- (0 until width).toList
        y <- 0 until height
        c = lines(y + 1)(x + 1)
        if c != '.'
      yield parseBlizzard(x, y, width, height, c)
      Valley(width, height, blizzards)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input24.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for valley <- is.runCollect.map(Parser.parseInput)
    yield valley.minPath

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
