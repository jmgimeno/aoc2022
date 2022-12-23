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

  enum Move(val directions: List[Direction], val action: Position => Position):
    case north extends Move(List(Direction.N, Direction.NE, Direction.NW), _ + Direction.N)
    case south extends Move(List(Direction.S, Direction.SE, Direction.SW), _ + Direction.S)
    case west extends Move(List(Direction.W, Direction.NW, Direction.SW), _ + Direction.W)
    case east extends Move(List(Direction.E, Direction.NE, Direction.SE), _ + Direction.E)

  enum ElveRound:
    case DoNothing(stay: Position)
    case Move(from: Position, to: Position)

  final case class State(
      numRound: Int,
      elves: Set[Position],
      moves: List[Move],
      prevElves: Set[Position] = Set.empty
  ):
    def emptyGround: Int =
      val minX = elves.minBy(_.x).x
      val maxX = elves.maxBy(_.x).x
      val minY = elves.minBy(_.y).y
      val maxY = elves.maxBy(_.y).y
      (maxX - minX + 1) * (maxY - minY + 1) - elves.size

  class Simulator(elves: Set[Position]):
    def run(finish: State => Boolean) =
      val initial = State(0, elves, List(Move.north, Move.south, Move.west, Move.east))
      LazyList
        .iterate(initial) { case State(step, elves, moves, _) =>
          val firstHalf = elves.iterator.map { elve =>
            if !Direction.values.map(elve + _).exists(elves)
            then ElveRound.DoNothing(elve)
            else
              moves
                .find { move => !move.directions.map(elve + _).exists(elves) }
                .map { move => ElveRound.Move(elve, move.action(elve)) }
                .getOrElse(ElveRound.DoNothing(elve))
          }.toList
          val counters = firstHalf
            .collect { case ElveRound.Move(_, to) => to }
            .groupMapReduce(identity)(_ => 1)(_ + _)
          val secondHalf =
            firstHalf.map {
              case ElveRound.DoNothing(stay)                  => stay
              case ElveRound.Move(_, to) if counters(to) == 1 => to
              case ElveRound.Move(from, _)                    => from
            }.toSet
          State(step + 1, secondHalf, moves.rotate, elves)
        }
        .dropWhile(state => !finish(state))
        .head

  lazy val inputStream =
    ZStream
      .fromFileName("data/input23.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part[A](finishWhen: State => Boolean)(result: State => A)(is: UStream[String]): Task[A] =
    for elves <-
        is.zipWithIndex
          .flatMap { (line, y) =>
            ZStream
              .fromIterable(line)
              .zipWithIndex
              .collect { case ('#', x) => Position(x.toInt, y.toInt) }
          }
          .runCollect
          .map(_.toSet)
    yield Simulator(elves).run(finishWhen).pipe(result)

  val part1 = part(_.numRound == 10)(_.emptyGround)
  val part2 = part(st => st.elves == st.prevElves)(_.numRound)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
