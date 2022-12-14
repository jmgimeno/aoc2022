import zio.*
import zio.stream.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day14 extends ZIOAppDefault:

  enum Segment:
    case Horizontal(left: Int, right: Int, at: Int)
    case Vertical(top: Int, bottom: Int, at: Int)
    case Bottom(at: Int)

    def yMax = this match
      case Horizontal(left, right, at) => at
      case Vertical(top, bottom, at)   => bottom
      case Bottom(at)                  => at

    def occupies(coord: Coord) = (this, coord) match
      case (Horizontal(l, r, a), Coord(x, y)) => a == y && l <= x && x <= r
      case (Vertical(t, b, a), Coord(x, y))   => a == x && t <= y && y <= b
      case (Bottom(a), Coord(x, y))           => y == a

  case class Rock(segments: List[Segment]):
    val yMax = segments.map(_.yMax).max

    def occupies(coord: Coord) =
      segments.exists(_.occupies(coord))

    def escapes(coord: Coord) =
      coord.y > yMax

    def eventHorizon =
      // Does not filter out any segment
      Rock {
        segments.filter { segment =>
          segment match
            case Segment.Horizontal(l, r, at) =>
              val eL = 500 - at
              val eR = 500 + at
              l <= eR || r >= eL
            case Segment.Vertical(top, bottom, at) =>
              val eY = math.abs(500 - at)
              eY <= bottom
            case _ => assert(false, "shouldn't happen")
        }
      }

  case class Coord(x: Int, y: Int):
    def steps =
      List(Coord(x, y + 1), Coord(x - 1, y + 1), Coord(x + 1, y + 1))

  object Parser:
    def parsePath(line: String): List[Segment] =
      line
        .split(" -> ")
        .map(parseCoords)
        .sliding(2)
        .map(segment => parseSegment(segment(0), segment(1)))
        .toList

    def parseSegment(first: Coord, second: Coord) = (first, second) match
      case (Coord(fx, fy), Coord(sx, sy)) =>
        if fx == sx then
          Segment.Vertical(math.min(fy, sy), math.max(fy, sy), fx)
        else if fy == sy then
          Segment.Horizontal(math.min(fx, sx), math.max(fx, sx), fy)
        else assert(false, "should'n happen")

    def parseCoords(coords: String) = coords match
      case s"$x,$y" => Coord(x.toInt, y.toInt)

  case class Simulator(rock: Rock):

    def simulatePart1 =
      val atRest = mutable.Set.empty[Coord]

      extension (coord: Coord)
        def forwardToRestOrEscaped =
          @tailrec def forward(coord: Coord): Coord =
            coord.steps.find(n => !atRest(n) && !rock.occupies(n)) match
              case Some(next) =>
                if rock.escapes(next) then next
                else forward(next)
              case None => coord
          forward(coord)

      @tailrec def loop: Int =
        val current = Coord(500, 0)
        val next = current.forwardToRestOrEscaped
        if rock.escapes(next) then atRest.size
        else
          atRest += next
          loop
      loop

    def simulatePart2 =
      val atRest = mutable.Set.empty[Coord]

      extension (coord: Coord)
        def forwardToRest =
          @tailrec def forward(coord: Coord): Coord =
            coord.steps.find(n => !atRest(n) && !rock.occupies(n)) match
              case Some(next) => forward(next)
              case None       => coord
          forward(coord)

      @tailrec def loop: Int =
        val current = Coord(500, 0)
        val next = current.forwardToRest
        if (current == next) then atRest.size + 1
        else
          atRest += next
          loop
      loop

  lazy val inputStream =
    ZStream
      .fromFileName("data/input14.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for rock <-
        is.flatMap(line => ZStream.fromIterable(Parser.parsePath(line)))
          .runCollect
          .map(_.toList.pipe(Rock.apply))
    yield Simulator(rock).simulatePart1

  def part2(is: UStream[String]): Task[Int] =
    for
      rock <-
        is.flatMap(line => ZStream.fromIterable(Parser.parsePath(line)))
          .runCollect
          .map(_.toList.pipe(Rock.apply))
      bottom = Segment.Bottom(rock.yMax + 2)
      newRock = Rock(bottom :: rock.segments)
    yield Simulator(newRock).simulatePart2

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
