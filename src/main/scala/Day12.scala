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
      values.findAll(_ == c).head

    def findAll(p: A => Boolean) =
      for
        row <- values.zipWithIndex.filter(_._1.exists(p)).map(_._2)
        col <- values(row).zipWithIndex.filter((a, _) => p(a)).map(_._2)
      yield Position(row, col)

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

    val neighbours = Bounds(heightMap.height, heightMap.width)

    def breathFirst(stopAt: Char => Boolean) =
      val open = mutable.Queue(heightMap.end -> 0)
      val seen = mutable.Set(heightMap.end)

      @tailrec def loop: Int =
        if open.isEmpty then Integer.MAX_VALUE
        else
          val (current, length) = open.dequeue()
          heightMap(current) match
            case rawValue if stopAt(rawValue) => length
            case rawValue =>
              val value = normalize(rawValue)
              neighbours(current)
                .filter { neighbour =>
                  normalize(heightMap(neighbour)) >= value - 1
                }
                .filterNot { seen }
                .foreach { neighbour =>
                  open += neighbour -> (length + 1)
                  seen += neighbour
                }
              loop
      loop

    def findMinPath(stopAt: Char => Boolean) =
      // we begin at the end
      val maxH = normalize(heightMap(heightMap.end))
      val g = mutable.Map(heightMap.end -> 0)
      val h = (p: Position) => maxH - normalize(heightMap(p))
      val f = mutable.Map(heightMap.end -> 0)

      val open = // priority queues do not have modification of priorities
        mutable.Set(heightMap.end)

      val seen = mutable.Set(heightMap.end)
      val path = mutable.Map.empty[Position, Position]

      @tailrec def loop: Int =
        if open.isEmpty then Integer.MAX_VALUE
        else
          val current = open.minBy(f).tap(open.remove)
          heightMap(current) match
            case rawValue if stopAt(rawValue) =>
              getpath(current, path.toMap).size
            case rawValue =>
              val value = normalize(rawValue)
              neighbours(current)
                .filter { neighbour =>
                  normalize(heightMap(neighbour)) >= value - 1
                }
                .foreach { neighbour =>
                  val score = g(current) + 1
                  if score < g.getOrElse(neighbour, Integer.MAX_VALUE) then
                    path(neighbour) = current
                    g(neighbour) = score
                    f(neighbour) = score + h(neighbour)
                    if !seen(neighbour)
                    then
                      open += neighbour
                      seen += current
                }
              loop
      loop

    private def getpath(
        start: Position,
        path: Map[Position, Position]
    ): List[Position] =
      @tailrec def loop(
          current: Position,
          tail: List[Position]
      ): List[Position] =
        if current == heightMap.end then tail
        else loop(path(current), current :: tail)
      loop(start, List.empty)

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

  def part(stopAt: Char => Boolean)(is: UStream[String]): Task[Int] =
    for
      input <- is.runCollect
      heightMap = Parser.parseMap(input)
    yield PathFinder(heightMap).findMinPath(stopAt)

  val part1 = part(Set('S'))
  val part2 = part(Set('S', 'a'))

  def partBF(stopAt: Char => Boolean)(is: UStream[String]): Task[Int] =
    for
      input <- is.runCollect
      heightMap = Parser.parseMap(input)
    yield PathFinder(heightMap).breathFirst(stopAt)

  val part1BF = partBF(Set('S'))
  val part2BF = partBF(Set('S', 'a'))

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
