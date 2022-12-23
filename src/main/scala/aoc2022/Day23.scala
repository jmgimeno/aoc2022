package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day23 extends ZIOAppDefault:

  extension [A](as: List[A])
    def rotate: List[A] = as match
      case head :: next => next :+ head
      case Nil          => Nil

  final case class Position(x: Int, y: Int):
    infix def +(direction: Direction) = Position(x + direction.dx, y + direction.dy)

  enum Direction(val dx: Int, val dy: Int):
    case N extends Direction(0, -1)
    case S extends Direction(0, +1)
    case W extends Direction(-1, 0)
    case E extends Direction(+1, 0)
    case NE extends Direction(+1, -1)
    case NW extends Direction(-1, -1)
    case SE extends Direction(+1, +1)
    case SW extends Direction(-1, +1)

  final case class Move(directions: Set[Direction], action: Position => Position)

  object Move:
    import Direction.*
    val north = Move(Set(N, NE, NW), _ + N)
    val south = Move(Set(S, SE, SW), _ + S)
    val west = Move(Set(W, NW, SW), _ + W)
    val east = Move(Set(E, NE, SE), _ + E)

  enum ElveRound:
    case DoNothing(stay: Position)
    case Move(from: Position, to: Position)

  class Scan(elves: Set[Position]):

    final case class State(elves: Set[Position], moves: List[Move]):
      def emptyGround: Int =
        val minX = elves.minBy(_.x).x
        val maxX = elves.maxBy(_.x).x
        val minY = elves.minBy(_.y).y
        val maxY = elves.maxBy(_.y).y
        (maxX - minX + 1) * (maxY - minY + 1) - elves.size

    def run(numSteps: Int) =
      val initial = State(elves, List(Move.north, Move.south, Move.west, Move.east))
      (1 to numSteps).foldLeft(initial) { case (State(elves, moves), step) =>
        val firstHalf = elves.toList.map { elve =>
          if !Direction.values.map(elve + _).exists(elves)
          then ElveRound.DoNothing(elve)
          else
            moves
              .find { case Move(directions, _) =>
                !directions.map(elve + _).exists(elves)
              }
              .map { case Move(_, action) =>
                ElveRound.Move(elve, action(elve))
              }
              .getOrElse(ElveRound.DoNothing(elve))
        }
        val counters = firstHalf
          .collect { case ElveRound.Move(_, to) => to }
          .groupMapReduce(identity)(_ => 1)(_ + _)
        val secondHalf =
          firstHalf.map {
            case ElveRound.DoNothing(stay)                  => stay
            case ElveRound.Move(_, to) if counters(to) == 1 => to
            case ElveRound.Move(from, _)                    => from
          }.toSet
        State(secondHalf, moves.rotate)
      }

  lazy val inputStream =
    ZStream
      .fromFileName("data/input23.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for elves <-
        is.zipWithIndex.flatMap { (line, y) =>
          ZStream
            .fromIterable(line)
            .zipWithIndex
            .collect { case ('#', x) => Position(x.toInt, y.toInt) }
        }.runCollect
    yield Scan(elves.toSet).run(10).emptyGround

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
