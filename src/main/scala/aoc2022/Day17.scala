package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec

object Day17 extends ZIOAppDefault:

  extension (b: Byte)
    def toBinStr =
      (List.fill(7)("0") ++
        List
          .unfold(b) { b =>
            if b != 0
            then Some((b % 2).toString, (b / 2).toByte)
            else None
          }
          .reverse).foldLeft("")(_ + _).takeRight(7)

  extension (bytes: List[Byte])
    def debug =
      bytes.map(_.toBinStr)

  extension [A](elements: IterableOnce[A])
    def cycle: LazyList[A] =
      LazyList.continually(elements).flatten

  extension [S](num: Long)
    def iterate(initial: S)(step: S => S): S =
      var i = 0
      var state = initial
      while (i < num) do
        state = step(state)
        i = i + 1
      state

  enum Move:
    case Left, Right

  object Move:
    def parse(c: Char) = c match
      case '<' => Left
      case '>' => Right

  case class Rock(bytes: List[Byte]):

    def tryMoveInsideBounds(move: Move): Option[Rock] = move match
      // 0 100 - 0000
      case Move.Left if bytes.forall(b => (b & 0x40) == 0) =>
        Some(Rock(bytes.map(b => (b << 1).toByte)))
      // 0 000 - 0001
      case Move.Right if bytes.forall(b => (b & 0x01) == 0) =>
        Some(Rock(bytes.map(b => (b >>> 1).toByte)))
      case _ => None

    def tryMoveHorizontal(background: List[Byte], move: Move): Option[Rock] =
      // assert(background.zip(bytes).forall((b, r) => (b & r) == 0), "we shouldn't have interference")
      tryMoveInsideBounds(move).filter { newRock =>
        background.zip(newRock.bytes).forall((b, r) => (b & r) == 0)
      }

    def moveToStop(
        lines: List[Byte],
        moves: LazyList[Move]
    ): (List[Byte], LazyList[Move], Int) =
      @tailrec def loop(
          rock: Rock,
          lines: List[Byte],
          moves: LazyList[Move],
          previous: List[Byte],
          background: List[Byte]
      ): (List[Byte], LazyList[Move], Int) =
        val nextMove #:: restMoves = moves: @unchecked
        val newRock = rock.tryMoveHorizontal(background, nextMove).getOrElse(rock)
        val newBackground = background.drop(1) :+ lines.head
        val canMoveDown = newBackground.zip(newRock.bytes).forall((b, r) => (b & r) == 0)
        if canMoveDown then
          val newPrevious = previous :+ background.head
          loop(newRock, lines.tail, restMoves, newPrevious, newBackground)
        else
          val fusedWithBackground = background.zip(newRock.bytes).map(_ | _).map(_.toByte)
          if fusedWithBackground.contains(0x7f)
          then (previous ++ fusedWithBackground, restMoves, lines.size)
          else (previous ++ fusedWithBackground ++ lines, restMoves, 0)

      loop(this, lines, moves, List.empty, List.fill(bytes.length)(0x00))

  object Rock:
    // ··####· 0x1E
    val dash = Rock(List(0x1e))
    // ···#··· 0x08
    // ··###·· 0x1C
    // ...#... 0x08
    val cross = Rock(List(0x08, 0x1c, 0x08))
    // ....#·· 0x04
    // ....#·· 0x04
    // ..###·· 0x1C
    val angle = Rock(List(0x04, 0x04, 0x1c))
    // ..#.... 0x10
    // ..#.... 0x10
    // ..#.... 0x10
    // ..#.... 0x10
    val needle = Rock(List(0x10, 0x10, 0x10, 0x10))
    // ..##... 0x18
    // ..##... 0x18
    val square = Rock(List(0x18, 0x18))
    val sequence = List(dash, cross, angle, needle, square).cycle

  case class Tower(lines: List[Byte]):

    def height: Int = lines.dropWhile(_ == 0x00).length - 1

    def add(rock: Rock, moves: LazyList[Move]): (Tower, LazyList[Move], Int) =
      val normalizeLines = List.fill[Byte](3)(0x00) ++ lines.dropWhile(_ == 0x00)
      val (newLines, restMoves, deleted) = rock.moveToStop(normalizeLines, moves)
      (Tower(newLines), restMoves, deleted)

  object Tower:
    def make = Tower(List(0x7f))

  case class State(tower: Tower, moves: LazyList[Move], rocks: LazyList[Rock], deleted: Int):
    def height = tower.height + deleted

  case class Simulate(moves: LazyList[Move], rocks: LazyList[Rock]):

    def runStep(state: State): State =
      val State(tower, moves, rock #:: restRocks, deleted) = state: @unchecked
      val (newTower, restMoves, newDeleted) = tower.add(rock, moves)
      State(newTower, restMoves, restRocks, deleted + newDeleted)

    def run(steps: Long): State =
      val initial = State(Tower.make, moves, rocks, 0)
      steps.iterate(initial)(runStep)

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
    for moveSequence <- is.map(Move.parse).runCollect.map(_.cycle)
    yield Simulate(moveSequence, Rock.sequence).run(1000000000000L).height

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
