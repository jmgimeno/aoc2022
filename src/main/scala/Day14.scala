import zio.*
import zio.stream.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day14 extends ZIOAppDefault:

  enum Segment:
    case Horizontal(left: Int, right: Int, at: Int)
    case Vertical(top: Int, bottom: Int, at: Int)

    def yMax = this match
      case Horizontal(left, right, at) => at
      case Vertical(top, bottom, at)   => bottom

    def occupies(coord: Coord) = (this, coord) match
      case (Horizontal(l, r, a), Coord(x, y)) => a == y && l <= x && x <= r
      case (Vertical(t, b, a), Coord(x, y))   => a == x && t <= y && y <= b

  case class Rock(segments: List[Segment]):
    val yMax = segments.map(_.yMax).max

    def occupies(coord: Coord) =
      segments.exists(_.occupies(coord))

    def escapes(coord: Coord) =
      coord.y > yMax

  case class Coord(x: Int, y: Int):
    private def steps =
      List(Coord(x, y + 1), Coord(x - 1, y + 1), Coord(x + 1, y + 1))

    def atRestOrEscaped(advance: Coord => Boolean): Coord =
      @tailrec def forward(coord: Coord): Coord =
        println(s"coord: $coord")
        coord.steps.find(advance) match
          case Some(next) => forward(next)
          case None       => coord
      forward(this)

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

  case class SimulationResult(unitsAtRest: Int)

  case class Simulator(rock: Rock):
    def run: SimulationResult =
      val atRest = mutable.Set.empty[Coord]
      @tailrec def loop: SimulationResult =
        val start = Coord(500, 0)
        assert(!atRest(start) && !rock.occupies(start), "start should be free")
        val stop =
          start.atRestOrEscaped(c =>
            !atRest(c) && !rock.occupies(c) && !rock.escapes(c)
          )
        println(s"start $start stop $stop")
        if atRest.size > 1 then SimulationResult(-1)
        if rock.escapes(stop) then SimulationResult(atRest.size)
        else
          atRest += stop
          println(s"atrest: $atRest")
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
    yield Simulator(rock).run.unitsAtRest

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
