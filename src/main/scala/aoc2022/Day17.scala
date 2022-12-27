package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec

object Day17 extends ZIOAppDefault:

  extension [A](elements: IterableOnce[A])
    def cycle: LazyList[A] =
      LazyList.continually(elements).flatten

  extension [S](range: Range)
    def iterate(initial: S)(step: S => S): S =
      range.foldLeft(initial)((s, _) => step(s))

  enum Move:
    case Left, Right

  object Move:
    def parse(c: Char) = c match
      case '<' => Left
      case '>' => Right

  case class Rock(bytes: Vector[Byte]):

    def tryMove(move: Move): Option[Rock] = move match
      case Move.Left  => tryMoveLeft
      case Move.Right => tryMoveRight

    private def tryMoveLeft =
      // 0 100 - 0000
      val canMoveLeft = bytes.forall(b => (b & 0x40) == 0)
      if canMoveLeft then Some(Rock(bytes.map(b => (b << 1).toByte)))
      else None

    private def tryMoveRight =
      // 0 000 - 0001
      val canMoveRight = bytes.forall(b => (b & 0x01) == 0)
      if canMoveRight then Some(Rock(bytes.map(b => (b >>> 1).toByte)))
      else None

    def move(background: Vector[Byte], move: Move): Rock =
      tryMove(move) match
        case Some(newRock) =>
          val canMove = background.zip(newRock.bytes).forall((b, r) => (b & r) == 0)
          if canMove then newRock else this
        case None => this

    def moveToStop(lines: Vector[Byte], moves: LazyList[Move]): (Vector[Byte], LazyList[Move]) =
      @tailrec def loop(
          rock: Rock,
          lines: Vector[Byte],
          moves: LazyList[Move],
          previous: Vector[Byte]
      ): (Vector[Byte], LazyList[Move]) =
        val nextMove #:: restMoves = moves: @unchecked
        // println(s"rock: $rock")
        // println(s"lines $lines")
        // println(s"previous: $previous")
        // println(s"WHOLE: ${previous ++ rock.bytes ++ lines}")
        // println(nextMove)
        val background = lines.take(bytes.size)
        val nextLines = lines.drop(bytes.size)
        val newRock = rock.move(background, nextMove)
        if nextLines.isEmpty then
          val stopAtBottom = background.zip(newRock.bytes).map(_ | _).map(_.toByte)
          (previous ++ stopAtBottom, restMoves)
        else
          val canMoveDown = (bytes.last & nextLines.head) == 0
          if canMoveDown then
            val newPrevious = previous :+ lines.head
            loop(newRock, lines.tail, restMoves, newPrevious)
          else
            val stopAtMiddle = background.zip(newRock.bytes).map(_ | _).map(_.toByte)
            (previous ++ stopAtMiddle ++ nextLines, restMoves)

      loop(this, lines, moves, Vector.empty)

  object Rock:
    val dash = Rock(Vector(30))
    val cross = Rock(Vector(8, 28, 8))
    val angle = Rock(Vector(4, 4, 28))
    val needle = Rock(Vector(16, 16, 16, 16))
    val square = Rock(Vector(24, 24))
    val sequence = List(dash, cross, angle, needle, square).cycle

  case class Tower(lines: Vector[Byte]):
    def height: Int = lines.size
    def add(rock: Rock, moves: LazyList[Move]): (Tower, LazyList[Move]) =
      val initLines = Vector.fill[Byte](rock.bytes.size + 3)(0) ++ lines.dropWhile(_ == 0)
      val (newLines, restMoves) = rock.moveToStop(initLines, moves)
      // println(s"newLines: $newLines")
      (Tower(newLines), restMoves)

  object Tower:
    def make = Tower(Vector.empty)

  case class State(tower: Tower, moves: LazyList[Move], rocks: LazyList[Rock])

  case class Simulate(moves: LazyList[Move], rocks: LazyList[Rock]):

    def runStep(state: State): State =
      val State(tower, moves, rock #:: restRocks) = state: @unchecked
      val (newTower, restMoves) = tower.add(rock, moves)
      State(newTower, restMoves, restRocks)

    def run(steps: Int): Tower =
      val initial = State(Tower.make, moves, rocks)
      (1 to steps).iterate(initial)(runStep).tower

  lazy val inputStream =
    ZStream
      .fromFileName("data/input17.txt")
      .via(ZPipeline.utf8Decode)
      .flatMap(ZStream.fromIterable)
      .orDie

  def part1(is: UStream[Char]): Task[Int] =
    for moveSequence <- is.map(Move.parse).runCollect.map(_.cycle)
    yield Simulate(moveSequence, Rock.sequence).run(2022).tap(println).height

  def part2(is: UStream[Char]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
