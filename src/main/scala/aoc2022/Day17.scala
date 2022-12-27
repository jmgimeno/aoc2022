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

    val masks = Vector.iterate(1, 7)(_ * 2)

    def tryMove(move: Move): Option[Rock] = move match
      case Move.Left  => tryMoveLeft
      case Move.Right => tryMoveRight

    private def tryMoveLeft =
      val canMoveLeft = bytes.forall(b => (b & masks(0)) != 1)
      if canMoveLeft then Some(Rock(bytes.map(b => (b >>> 1).toByte)))
      else None

    private def tryMoveRight =
      val canMoveRight = bytes.forall(b => (b & masks(6)) != 1)
      if canMoveRight then Some(Rock(bytes.map(b => (b << 1).toByte)))
      else None

    def move(background: Vector[Byte], move: Move): Rock =
      tryMove(move) match
        case Some(newRock) =>
          val canMove = background.zip(newRock.bytes).forall((b, r) => (b & r) == 0)
          if canMove then newRock else this
        case None => this

    def moveToStop(lines: Vector[Byte], moves: LazyList[Move]): (Vector[Byte], LazyList[Move]) =
      @tailrec def loop(
          lines: Vector[Byte],
          moves: LazyList[Move],
          previous: Vector[Byte]
      ): (Vector[Byte], LazyList[Move]) =
        val nextMove #:: restMoves = moves: @unchecked
        val prefix = lines.take(bytes.size)
        val newRock = move(prefix, nextMove)
        if lines.size == bytes.size then
          val stopAtBottom = prefix.zip(newRock.bytes).map(_ | _).map(_.toByte)
          (previous ++ stopAtBottom, restMoves)
        else
          val canMoveDown = (bytes.last & lines(bytes.size)) == 0
          if canMoveDown then
            val newPrevious = previous :+ lines(0)
            loop(lines.tail, restMoves, newPrevious)
          else
            val stopAtMiddle = prefix.zip(newRock.bytes).map(_ | _).map(_.toByte)
            (previous ++ stopAtMiddle ++ lines.drop(bytes.size), restMoves)

      loop(lines, moves, Vector.empty)

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
      val initLines = Vector[Byte](0, 0, 0) ++ lines.dropWhile(_ == 0)
      val (newLines, restMoves) = rock.moveToStop(initLines, moves)
      (Tower(newLines), restMoves)

  object Tower:
    def make = Tower(Vector.empty)

  case class State(tower: Tower, moves: LazyList[Move], rocks: LazyList[Rock])

  case class Simulate(moves: LazyList[Move], rocks: LazyList[Rock]):

    private def forceThreeEmpties(tower: Tower): Tower =
      Tower(Vector[Byte](0, 0, 0) ++ tower.lines.dropWhile(_ == 0))

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
    yield Simulate(moveSequence, Rock.sequence).run(2022).height

  def part2(is: UStream[Char]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
