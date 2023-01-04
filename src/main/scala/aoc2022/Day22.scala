package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps
import java.util.StringTokenizer
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

object Day22 extends ZIOAppDefault:

  extension [A](matrix: Array[Array[A]])
    def apply(position: Position) = Try {
      matrix(position.y)(position.x)
    }
    def columnsAt(position: Position) =
      matrix(position.y).length
    def rowsAt =
      matrix.length

  enum Tile:
    case Null, Open, Wall

  case class Notes private (tiles: Array[Array[Tile]]):
    def ahead(status: Status, n: Int): Position =
      val Status(position @ Position(x, y), orientation) = status
      @tailrec def loop(current: Position, lastOpen: Position, steps: Int): Position =
        if steps == n then lastOpen
        else
          val nextPosition = current + orientation
          tiles(nextPosition) match
            case Failure(exception) =>
              orientation match
                case Orientation.Right =>
                  loop(nextPosition.copy(x = -1), lastOpen, steps)
                case Orientation.Down =>
                  loop(nextPosition.copy(y = -1), lastOpen, steps)
                case Orientation.Left =>
                  val lastColumn = tiles.columnsAt(nextPosition)
                  loop(nextPosition.copy(x = lastColumn), lastOpen, steps)
                case Orientation.Up =>
                  val lastRow = tiles.rowsAt
                  loop(nextPosition.copy(y = lastRow), lastOpen, steps)
            case Success(Tile.Null) => loop(nextPosition, lastOpen, steps)
            case Success(Tile.Open) => loop(nextPosition, nextPosition, steps + 1)
            case Success(Tile.Wall) => lastOpen
      loop(position, position, 0)

  object Notes:
    def make(tiles: Array[Array[Tile]]) =
      val maxLength = tiles.map(_.length).max
      val normalized = tiles.map(row => row ++ Array.fill(maxLength - row.length)(Tile.Null))
      Notes(normalized)

  enum Step:
    case Ahead(n: Int)
    case Clockwise
    case CounterClockwise

  case class Path(steps: List[Step])

  case class Position(x: Int, y: Int):
    infix def +(orientation: Orientation): Position =
      Position(x + orientation.dx, y + orientation.dy)
    infix def -(other: Position): Position =
      Position(x - other.x, y - other.y)

  enum Orientation(val dx: Int, val dy: Int):
    case Right extends Orientation(1, 0)
    case Down extends Orientation(0, 1)
    case Left extends Orientation(-1, 0)
    case Up extends Orientation(0, -1)
    def counterClockWise = this match
      case Right => Up
      case Down  => Right
      case Left  => Down
      case Up    => Left
    def clockWise = this match
      case Right => Down
      case Down  => Left
      case Left  => Up
      case Up    => Right

  case class Status(position: Position, orientation: Orientation):
    def finalPassword =
      val row = position.y + 1
      val col = position.x + 1
      val orient = orientation.ordinal
      1000 * row + 4 * col + orient

  class Walker(notes: Notes):
    val start = Status(Position(notes.tiles(0).indexWhere(_ != Tile.Null), 0), Orientation.Right)
    def walk(path: Path): Int =
      path.steps
        .foldLeft(start) { case (status @ Status(position, orientation), step) =>
          step match
            case Step.Ahead(n)         => Status(notes.ahead(status, n), orientation)
            case Step.Clockwise        => Status(position, orientation.clockWise)
            case Step.CounterClockwise => Status(position, orientation.counterClockWise)
        }
        .finalPassword

  object Parser:

    def parseTile(c: Char) = c match
      case ' ' => Tile.Null
      case '.' => Tile.Open
      case '#' => Tile.Wall

    def parseStep(step: String): Step = step match
      case "R" => Step.Clockwise
      case "L" => Step.CounterClockwise
      case n   => Step.Ahead(n.toInt)

    def parseNotes(block: Chunk[String]): Notes =
      Notes.make(Array.from(block.map(line => Array.from(line.map(parseTile)))))

    def parsePath(block: Chunk[String]): Path =
      val st = new StringTokenizer(block(0), "RL", true)
      Path {
        Iterator
          .unfold(st) { st =>
            if st.hasMoreTokens() then Some((st.nextToken(), st)) else None
          }
          .map(parseStep)
          .toList
      }

  lazy val inputStream =
    ZStream
      .fromFileName("data/input22.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for
      blocks <- is.split(_.isEmpty).runCollect
      notes = Parser.parseNotes(blocks(0))
      path = Parser.parsePath(blocks(1))
    yield Walker(notes).walk(path)

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
