package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
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
    ): List[Byte] =
      @tailrec def loop(
          rock: Rock,
          lines: List[Byte],
          previous: List[Byte],
          background: List[Byte]
      ): List[Byte] =
        val nextMove = moves.next
        val newRock = rock.tryMoveHorizontal(background, nextMove).getOrElse(rock)
        val newBackground = background.drop(1) :+ lines.head
        val canMoveDown = newBackground.zip(newRock.bytes).forall((b, r) => (b & r) == 0)
        if canMoveDown then
          val newPrevious = previous :+ background.head
          loop(newRock, lines.tail, newPrevious, newBackground)
        else
          val fusedWithBackground = background.zip(newRock.bytes).map(_ | _).map(_.toByte)
          previous ++ fusedWithBackground ++ lines

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

    def shape: List[Int] =
      val normalizeLines = lines.dropWhile(_ == 0x00)
      List.tabulate(7) { pos =>
        normalizeLines.indexWhere { b =>
          (b >> pos) % 2 == 1
        }
      }

    def add(rock: Rock, moves: Cycle[Move]): Tower =
      val normalizeLines = List.fill[Byte](3)(0x00) ++ lines.dropWhile(_ == 0x00)
      val newLines = rock.moveToStop(normalizeLines, moves)
      Tower(newLines)

  object Tower:
    def make = Tower(List(0x7f))

  case class Simulate(moves: Cycle[Move], rocks: Cycle[Rock]):

    case class Memento(idRock: Int, idMove: Int, shape: List[Int])

    def run(steps: Long): Long =
      val cache = mutable.Map[Memento, Long]()
      val heights = mutable.Map[Long, Long]()
      @tailrec def loop(t: Long, tower: Tower): Long =
        if t == steps then tower.height
        else
          val rock = rocks.next
          val newTower = tower.add(rock, moves)
          val memento = Memento(rocks.index, moves.index, newTower.shape)
          if !cache.contains(memento) then
            cache += memento -> t
            heights += t -> newTower.height
            loop(t + 1, newTower)
          else
            val tPrevious = cache(memento)
            val hPrevious = heights(tPrevious)
            val remaining = steps - t
            val period = t - tPrevious
            val increment = newTower.height - hPrevious
            val cycles = remaining / period
            val diff = remaining % period
            val hDiff = heights(tPrevious + diff) - hPrevious
            newTower.height + increment * cycles + hDiff
      loop(1, Tower.make)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input17.txt")
      .via(ZPipeline.utf8Decode)
      .flatMap(ZStream.fromIterable)
      .orDie

  def part1(is: UStream[Char]): Task[Long] =
    for moveSequence <- is.map(Move.parse).runCollect.map(cm => Cycle(cm.toList))
    yield Simulate(moveSequence, Rock.sequence).run(2022)

  def part2(is: UStream[Char]): Task[Long] =
    for moveSequence <- is.map(Move.parse).runCollect.map(cm => Cycle(cm.toList))
    yield Simulate(moveSequence, Rock.sequence).run(1000000000000L)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
