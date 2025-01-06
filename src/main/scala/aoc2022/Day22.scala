package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps
import java.util.StringTokenizer
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

object Day22 extends ZIOAppDefault {

  enum Tile(symbol: Char) {
    case Open extends Tile('.')
    case Wall extends Tile('#')

    override def toString: String = symbol.toString
  }

  enum Step {
    case Ahead(n: Int)
    case Clockwise
    case CounterClockwise
  }

  import Step.*

  case class Path(steps: List[Step])

  case class Position(x: Int, y: Int) {
    infix def +(orientation: Orientation): Position =
      Position(x + orientation.dx, y + orientation.dy)
    infix def -(other: Position): Position =
      Position(x - other.x, y - other.y)
  }

  enum Orientation(val dx: Int, val dy: Int) {
    case Right extends Orientation(1, 0)
    case Down extends Orientation(0, 1)
    case Left extends Orientation(-1, 0)
    case Up extends Orientation(0, -1)

    def clockwise: Orientation = this match {
      case Right => Down
      case Down  => Left
      case Left  => Up
      case Up    => Right
    }

    def counterClockwise: Orientation = this match {
      case Right => Up
      case Down  => Right
      case Left  => Down
      case Up    => Left
    }
  }

  import Orientation.*

  class Face(val id: Int, val bigX: Int, val bigY: Int, tiles: Array[Array[Tile]]) {

    val size = tiles.length

    def apply(y: Int)(x: Int): Tile = tiles(y)(x)

    def bigPosition(position: Position): Position = {
      Position(bigX * size + position.x + 1, bigY * size + position.y + 1)
    }

    def isInside(position: Position): Boolean = {
      position.x >= 0 && position.x < size && position.y >= 0 && position.y < size
    }

    def password(position: Position): Int = {
      val totalPosition = bigPosition(position)
      1000 * totalPosition.y + 4 * totalPosition.x
    }

    override def toString: String =
      s"id: $id, bigX: $bigX, bigY: $bigY, size: $size\n" +
        tiles.map(_.mkString).mkString("\n")
  }

  case class Walker(val face: Face, val position: Position, val orientation: Orientation) {
    def password: Int = face.password(position) + orientation.ordinal
    override def toString: String = s"face: ${face.id}, position: $position, orientation: $orientation"
  }

  class Board(val faces: Array[Face], faceMap: Map[Int, Map[Orientation, Int]]) {
    def apply(f: Int)(y: Int)(x: Int): Tile = faces(f)(y)(x)

    def step(walker: Walker): Option[(Face, Position)] = {
      val nextPosition = walker.position + walker.orientation
      if walker.face.isInside(nextPosition) then 
        if walker.face(nextPosition.y)(nextPosition.x) == Tile.Wall then None
        else Some((walker.face, nextPosition))
      else 
        val newFace = faces(faceMap(walker.face.id)(walker.orientation))
        val newPosition = Position(
          if nextPosition.x < 0 then newFace.size - 1 else if nextPosition.x >= newFace.size then 0 else nextPosition.x,
          if nextPosition.y < 0 then newFace.size - 1 else if nextPosition.y >= newFace.size then 0 else nextPosition.y
        )
        if newFace(newPosition.y)(newPosition.x) == Tile.Wall then None
        else Some((newFace, newPosition))
    }

    def run(path: Path): Int = {

      @tailrec
      def go(walker: Walker, path: List[Step]): Walker = {
        path match
          case Nil => walker
          case Ahead(0) :: next =>
            go(walker, next)
          case Ahead(n) :: next => 
            step(walker) match
              case None => 
                go(walker, next)
              case Some((face, position)) => 
                go(Walker(face, position, walker.orientation), Ahead(n - 1) :: next)
          case Clockwise :: next => 
            go(walker.copy(orientation = walker.orientation.clockwise), next)
          case CounterClockwise :: next => 
            go(walker.copy(orientation = walker.orientation.counterClockwise), next)
      }

      val start = Walker(findStart(faces), Position(0, 0), Right)
      val end = go(start, path.steps)
      end.password
    }

    def findStart(faces: Array[Face]): Face = {
      faces.filter(_.bigY == 0).minBy(_.bigX)
    }

    override def toString: String =
      faces.map(_.toString).mkString("\n")
  }

  object Parser {

    def parseTile(c: Char) = {
      c match
        case '.' => Tile.Open
        case '#' => Tile.Wall
    }

    def parseStep(step: String): Step = {
      step match
        case "R" => Step.Clockwise
        case "L" => Step.CounterClockwise
        case n   => Step.Ahead(n.toInt)
    }

    def parseBoard(
        block: Chunk[String],
        shape: String,
        size: Int,
        faceMap: Map[Int, Map[Orientation, Int]]
    ): Board = {
      val tiles = Array.ofDim[Tile](6, size, size)

      def position(shape: String, f: Int): (Int, Int) = {
        val y = shape.split("\n").indexWhere(_.contains(f.toString))
        val x = shape.split("\n")(y).indexOf(f.toString)
        (x, y)
      }

      def parseFace(f: Int, bigX: Int, bigY: Int): Unit = {
        val minY = bigY * size
        val minX = bigX * size
        for (y <- 0 until size) {
          val line = block(minY + y)
          for (x <- 0 until size) {
            val c = line(minX + x)
            tiles(f)(y)(x) = parseTile(c)
          }
        }
      }

      var faces = Array.ofDim[Face](6)
      for (f <- 0 until 6) {
        val (bigX, bigY) = position(shape, f)
        parseFace(f, bigX, bigY)
        faces(f) = new Face(f, bigX, bigY, tiles(f))
      }

      Board(faces, faceMap)
    }

    def parsePath(block: Chunk[String]): Path = {
      val st = new StringTokenizer(block(0), "RL", true)
      Path {
        Iterator
          .unfold(st) { st =>
            if st.hasMoreTokens() then Some((st.nextToken(), st)) else None
          }
          .map(parseStep)
          .toList
      }
    }
  }

  lazy val inputStream = {
    ZStream
      .fromFileName("data/input22.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie
  }

  def part1(
      is: UStream[String],
      shape: String,
      size: Int,
      faceMap: Map[Int, Map[Orientation, Int]]
  ): Task[Int] = {
    for {
      blocks <- is.split(_.isEmpty).runCollect
      board = Parser.parseBoard(blocks(0), shape, size, faceMap)
      path = Parser.parsePath(blocks(1))
    } yield board.run(path)
  }

  def part2(is: UStream[String]): Task[Int] = {
    ZIO.succeed(-1)
  }

  val inputShape =
    """ 01
      | 2
      |34
      |5""".stripMargin

  val inputSize = 50

  val inputFaceMap: Map[Int, Map[Orientation, Int]] =
    Map(
      0 -> Map(Right -> 1, Down -> 2, Left -> 1, Up -> 4),
      1 -> Map(Right -> 0, Down -> 1, Left -> 0, Up -> 1),
      2 -> Map(Right -> 2, Down -> 4, Left -> 2, Up -> 0),
      3 -> Map(Right -> 4, Down -> 5, Left -> 4, Up -> 5),
      4 -> Map(Right -> 3, Down -> 0, Left -> 3, Up -> 2),
      5 -> Map(Right -> 5, Down -> 3, Left -> 5, Up -> 3)
    )

  lazy val run = {
    part1(inputStream, inputShape, inputSize, inputFaceMap).debug("PART1") *> part2(inputStream)
      .debug("PART2")
  }
}
