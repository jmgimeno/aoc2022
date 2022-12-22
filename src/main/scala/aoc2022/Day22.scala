package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps
import java.util.StringTokenizer

object Day22 extends ZIOAppDefault:

  enum Tile:
    case Null, Open, Wall

  case class Graph(nodes: mutable.Map[Position, Node])
  case class Node(neighours: mutable.Map[Orientation, Node])

  object Node:
    def make = Node(mutable.Map.empty)

  object Graph:
    def fromTiles(start: Position, notes: Notes) =
      val nodes = mutable.Map(start -> Node.make)
      val expanded = mutable.Set[Position]()
      def loop(fringe: List[Position]): Unit =
        fringe match
          case head :: next =>
            val node = nodes(head)
            Orientation.values.foreach { orientation =>
              val maybeNeighbour = orientation.step(head)
              notes.tileAt(maybeNeighbour) match
                case Tile.Null => ???
                case Tile.Open =>
                  node.neighours(orientation) = nodes.getOrElseUpdate(maybeNeighbour, Node.make)
                case Tile.Wall => ()
            }
            expanded += head
            loop(next)
          case Nil => ()
      loop(List(start))

  case class Notes(tiles: Array[Array[Tile]]):
    def tileAt(position: Position) =
      // do wrap-around ???
      tiles(position.y)(position.x)

    def steps(position: Position) =
      val Position(x, y) = position
      Orientation.values
        .map {
          case Orientation.Right =>
            if x >= tiles.size then ???
            else Some(Orientation.Right -> Position(x + 1, y))
          case Orientation.Down => ???
          case Orientation.Left => ???
          case Orientation.Up   => ???
        }
        .collect { case Some(a) =>
          a
        }
        .toMap

    def ahead(n: Int, status: Status): Position =
      val Status(position, orientation) = status
      assert(tileAt(position) == Tile.Open, "we only should step on valid tiles")
      orientation match
        case Orientation.Right => moveRight(position, n)
        case Orientation.Down  => moveDown(position, n)
        case Orientation.Left  => moveLeft(position, n)
        case Orientation.Up    => moveUp(position, n)

    private def moveRight(position: Position, n: Int): Position = ???
    private def moveDown(position: Position, n: Int): Position = ???
    private def moveLeft(position: Position, n: Int): Position = ???
    private def moveUp(position: Position, n: Int): Position = ???

  enum Step:
    case Ahead(n: Int)
    case Clockwise
    case CounterClockwise

  enum Orientation:
    case Right, Down, Left, Up
    def clockwise: Orientation = this match
      case Orientation.Right => Orientation.Down
      case Orientation.Down  => Orientation.Left
      case Orientation.Left  => Orientation.Up
      case Orientation.Up    => Orientation.Right

    def counterClockwise = this match
      case Orientation.Right => Orientation.Up
      case Orientation.Down  => Orientation.Right
      case Orientation.Left  => Orientation.Down
      case Orientation.Up    => Orientation.Left

    def step(p: Position): Position = this match
      case Orientation.Right => p.copy(x = p.x + 1)
      case Orientation.Down  => p.copy(y = p.y + 1)
      case Orientation.Left  => p.copy(x = p.x - 1)
      case Orientation.Up    => p.copy(y = p.y - 1)

  case class Path(steps: List[Step])

  case class Position(x: Int, y: Int):
    infix def -(other: Position): Position =
      Position(x - other.x, y - other.y)

  case class Status(position: Position, orientation: Orientation):
    def finalPassword(ref: Position) =
      val diff = position - ref
      val row = diff.y + 1
      val col = diff.x + 1
      val orient = orientation.ordinal
      1000 * row + 4 * col + orient

  case class Mover(notes: Notes, path: Path):

    val start = Status(Position(notes.tiles(0).indexWhere(_ != Tile.Null), 0), Orientation.Right)

    def finalPassword: Int =
      path.steps
        .foldLeft(start) { case (status @ Status(position, orientation), step) =>
          step match
            case Step.Ahead(n)         => Status(notes.ahead(n, status), orientation)
            case Step.Clockwise        => Status(position, orientation.clockwise)
            case Step.CounterClockwise => Status(position, orientation.counterClockwise)

        }
        .finalPassword(start.position)

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
      Notes(Array.from(block.map(line => Array.from(line.map(parseTile)))))

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
    yield Mover(notes, path).finalPassword

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
