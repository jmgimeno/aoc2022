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

  class Cycle[A](elements: List[A]):
    var index = 0
    def next: A =
      val result = elements(index)
      index = (index + 1) % elements.size
      result

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
        moves: Cycle[Move]
    ): (List[Byte], Int) =
      @tailrec def loop(
          rock: Rock,
          lines: List[Byte],
          previous: List[Byte],
          background: List[Byte]
      ): (List[Byte], Int) =
        val nextMove = moves.next
        val newRock = rock.tryMoveHorizontal(background, nextMove).getOrElse(rock)
        val newBackground = background.drop(1) :+ lines.head
        val canMoveDown = newBackground.zip(newRock.bytes).forall((b, r) => (b & r) == 0)
        if canMoveDown then
          val newPrevious = previous :+ background.head
          loop(newRock, lines.tail, newPrevious, newBackground)
        else
          val fusedWithBackground = background.zip(newRock.bytes).map(_ | _).map(_.toByte)
          if fusedWithBackground.contains(0x7f)
          then (previous ++ fusedWithBackground, lines.size)
          else (previous ++ fusedWithBackground ++ lines, 0)

      loop(this, lines, List.empty, List.fill(bytes.length)(0x00))

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
    def sequence = Cycle(List(dash, cross, angle, needle, square))

  case class Tower(lines: List[Byte]):

    def height: Int = lines.dropWhile(_ == 0x00).length - 1

    def add(rock: Rock, moves: Cycle[Move]): (Tower, Int) =
      val normalizeLines = List.fill[Byte](3)(0x00) ++ lines.dropWhile(_ == 0x00)
      val (newLines, deleted) = rock.moveToStop(normalizeLines, moves)
      (Tower(newLines), deleted)

  object Tower:
    def make = Tower(List(0x7f))

  case class State(tower: Tower, deleted: Int):
    def height = tower.height + deleted

  case class Simulate(moves: Cycle[Move], rocks: Cycle[Rock]):

    def runStep(state: State): State =
      val State(tower, deleted) = state: @unchecked
      val rock = rocks.next
      val (newTower, newDeleted) = tower.add(rock, moves)
      State(newTower, deleted + newDeleted)

    def run(steps: Long): State =
      val initial = State(Tower.make, 0)
      steps.iterate(initial)(runStep)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input17.txt")
      .via(ZPipeline.utf8Decode)
      .flatMap(ZStream.fromIterable)
      .orDie

  def part1(is: UStream[Char]): Task[Int] =
    for moveSequence <- is.map(Move.parse).runCollect.map(cm => Cycle(cm.toList))
    yield Simulate(moveSequence, Rock.sequence).run(2022).height

  def part2(is: UStream[Char]): Task[Int] =
    for moveSequence <- is.map(Move.parse).runCollect.map(cm => Cycle(cm.toList))
    yield Simulate(moveSequence, Rock.sequence).run(1000000000000L).height

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
