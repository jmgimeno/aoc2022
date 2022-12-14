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

    def breathFirst(start: Position) =
      val open = mutable.Queue(start -> 0)
      val seen = mutable.Set(start)

      @tailrec def loop: Int =
        if open.isEmpty then Integer.MAX_VALUE
        else
          val (current, length) = open.dequeue()
          heightMap(current) match
            case 'E' => length
            case rawValue =>
              val value = normalize(rawValue)
              neighbours(current)
                .filter { neighbour =>
                  normalize(heightMap(neighbour)) <= value + 1
                }
                .filterNot { seen }
                .foreach { neighbour =>
                  open += neighbour -> (length + 1)
                  seen += neighbour
                }
              loop
      loop

    def findMinPath(start: Position) =
      val g = mutable.Map(start -> 0)
      val h1 = (p: Position) => p.distance(heightMap.end)
      val h2 = (p: Position) => 'z' - normalize(heightMap(p))
      val h3 = (p: Position) => math.max(h1(p), h2(p))
      val h4 = (p: Position) => 0
      val h = h3
      val f =
        mutable.Map(start -> h(start))

      val open = // priority queues do not have modification of priorities
        mutable.Set(start)

      val expanded = mutable.Set.empty[Position]

      val path =
        mutable.Map.empty[Position, Position]

      @tailrec def loop: Int =
        if open.isEmpty then Integer.MAX_VALUE
        else
          val current = open.minBy(f).tap(open.remove)
          expanded += current
          heightMap(current) match
            case 'E' => getpath(start, path.toMap).size
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

    private def getpath(
        start: Position,
        path: Map[Position, Position]
    ): List[Position] =
      @tailrec def loop(
          current: Position,
          tail: List[Position]
      ): List[Position] =
        if current == start then tail
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

  def part(starting: Char => Boolean)(is: UStream[String]): Task[Int] =
    for
      input <- is.runCollect
      heightMap = Parser.parseMap(input)
      start = heightMap.points.findAll(starting)
      finder = PathFinder(heightMap)
      results <- ZIO.foreachPar(start)(s => ZIO.succeed(finder.findMinPath(s)))
    yield results.min

  val part1 = part(Set('S'))
  val part2 = part(Set('S', 'a'))

  def partBF(starting: Char => Boolean)(is: UStream[String]): Task[Int] =
    for
      input <- is.runCollect
      heightMap = Parser.parseMap(input)
      start = heightMap.points.findAll(starting)
      finder = PathFinder(heightMap)
      results <- ZIO.foreachPar(start)(s => ZIO.succeed(finder.breathFirst(s)))
    yield results.min

  val part1BF = partBF(Set('S'))
  val part2BF = partBF(Set('S', 'a'))

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
