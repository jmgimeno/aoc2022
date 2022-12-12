import zio.*
import zio.stream.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day12 extends ZIOAppDefault:

  type Height = Char
  type Move = Char

  extension [A](values: Array[Array[A]])
    def debug = values.map(_.mkString).mkString("\n")

    def find(c: A) =
      val row = values.indexWhere(_.contains(c))
      val col = values(row).indexWhere(_ == c)
      Position(row, col)

    def apply(pos: Position) =
      val Position(row, col) = pos
      values(row)(col)

    def updated(pos: Position, value: A) =
      val Position(row, col) = pos
      values(row)(col) = value

  case class HeightMap(points: Array[Array[Height]]):
    val start = points.find('S')
    val end = points.find('E')
    val height = points.size
    val width = points(0).size

    def apply(pos: Position) = points(pos)

  object Parser:
    def parseMap(input: Chunk[String]) =
      HeightMap {
        input.map { line =>
          line.toCharArray
        }.toArray
      }

  case class Position(row: Int, col: Int):
    def distance(other: Position) =
      math.abs(row - other.row) + math.abs(col - other.col)

  case class Bounds(height: Int, width: Int):
    def apply(point: Position) =
      for
        col <- point.col - 1 to point.col + 1
        if 0 <= col && col < width
        row <- point.row - 1 to point.row + 1
        if 0 <= row && row < height
        if row == point.row || col == point.col
        if row != point.row || col != point.col
      yield Position(row, col)

  case class Candidate(
      current: Position,
      f: Int
  )

  case class PathFinder(heightMap: HeightMap):

    def findMinPath =
      val neighbours = Bounds(heightMap.height, heightMap.width)
      val g = mutable.Map(heightMap.start -> 0)
      val h1 = (p: Position) => p.distance(heightMap.end)
      val h2 = (p: Position) => 'z' - normalize(heightMap(p))
      val h3 = (p: Position) => math.max(h1(p), h2(p))
      val h4 = (p: Position) => 0
      val h = h3
      val f =
        mutable.Map(heightMap.start -> h(heightMap.start))

      val open = // priority queues do not have modification of priorities
        mutable.Set(heightMap.start)

      val expanded = mutable.Set.empty[Position]

      val path =
        mutable.Map.empty[Position, Position]

      @tailrec def loop: List[Position] =
        if open.isEmpty then assert(false, "Haven't found a path")
        else
          val current = open.minBy(f).tap(open.remove)
          expanded += current
          heightMap(current) match
            case 'E' => getpath(path.toMap)
            case rawValue =>
              val value = normalize(rawValue)
              neighbours(current)
                .filter { neighbour =>
                  val newValue = normalize(heightMap(neighbour))
                  newValue <= value + 1
                }
                .foreach { neighbour =>
                  val score = g(current) + 1
                  if score < g.getOrElse(neighbour, Integer.MAX_VALUE) then
                    path(neighbour) = current
                    g(neighbour) = score
                    f(neighbour) = score + h(neighbour)
                    if !expanded.contains(neighbour) then open += neighbour
                }
              loop
      loop

    private def getpath(path: Map[Position, Position]): List[Position] =
      @tailrec def loop(
          current: Position,
          tail: List[Position]
      ): List[Position] =
        if current == heightMap.start then tail
        else loop(path(current), current :: tail)
      loop(heightMap.end, List.empty)

    private def normalize(value: Char) =
      value match
        case 'S' => 'a'
        case 'E' => 'z'
        case any => any

  val inputStream =
    ZStream
      .fromFileName("data/input12.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for
      input <- is.runCollect
      heightMap = Parser.parseMap(input)
    yield PathFinder(heightMap)
      .tap { h =>
        // println(h.heightMap.points.debug)
        // println(h.heightMap.start)
        // println(h.heightMap.end)
        // println(h.heightMap.height)
        // println(h.heightMap.width)
      }
      .findMinPath
      .size

  def part2(is: UStream[String]): Task[Int] =
    ???

  lazy val run =
    part1(inputStream).debug("PART1")
