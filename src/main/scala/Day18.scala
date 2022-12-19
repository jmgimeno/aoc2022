import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day18 extends ZIOAppDefault:

  case class Cube(x: Int, y: Int, z: Int):
    def contactsFace(other: Cube) =
      assert(this != other, "different")
      (x == other.x && y == other.y && (z == other.z - 1 || z == other.z + 1))
      || (x == other.x && (y == other.y - 1 || y == other.y + 1) && z == other.z)
      || ((x == other.x - 1 || x == other.x + 1) && y == other.y && z == other.z)

  object Cube:
    def parse(line: String) = line match
      case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input18.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  case class AreaCounter private (
      counters: Map[Cube, Int] = Map.empty,
      groups: Map[Cube, Int] = Map.empty
  ):
    def surfaceArea = counters.values.sum
    def numGroups = groups.values.toSet.size
    def clusters = groups
    def addCube(cube: Cube): AreaCounter =
      val initial =
        AreaCounter(counters.updated(cube, 6), groups.updated(cube, groups.size + 1))
      counters
        .foldLeft(initial) { case (ac @ AreaCounter(counters, groups), (other, counter)) =>
          if other.contactsFace(cube) then
            AreaCounter(
              counters
                .updated(other, counter - 1)
                .updated(cube, counters(cube) - 1),
              groups.updated(cube, groups(other))
            )
          else ac
        }

  object AreaCounter:
    def make = AreaCounter(Map.empty, Map.empty)

  case class UnreachableCounter(cubes: Set[Cube]):
    val minX = cubes.minBy(_.x).x
    val minY = cubes.minBy(_.y).y
    val minZ = cubes.minBy(_.z).z
    val maxX = cubes.maxBy(_.x).x
    val maxY = cubes.maxBy(_.y).y
    val maxZ = cubes.maxBy(_.z).z
    val total = (maxX - minX + 3) * (maxY - minY + 3) * (maxZ - minZ + 3)

    def surfaceAreaWithoutPockets: Int =

      def reachableIfMakesSense(outside: mutable.Set[Cube])(c: Cube): Unit =
        if !(c.x < minX - 1 || c.y < minY - 1 || c.z < minZ - 1
            || c.x > maxX + 1 || c.y > maxY + 1 || c.z > maxZ + 1
            || cubes(c))
        then reachable(outside)(c)

      def reachable(outside: mutable.Set[Cube])(c: Cube): Unit =
        if !cubes(c) && !outside(c) then
          outside += c
          reachableIfMakesSense(outside)(c.copy(x = c.x - 1))
          reachableIfMakesSense(outside)(c.copy(x = c.x + 1))
          reachableIfMakesSense(outside)(c.copy(y = c.y - 1))
          reachableIfMakesSense(outside)(c.copy(y = c.y + 1))
          reachableIfMakesSense(outside)(c.copy(z = c.z - 1))
          reachableIfMakesSense(outside)(c.copy(z = c.z + 1))

      def calculateSurfaceArea(cubes: Set[Cube]) =
        cubes.foldLeft(AreaCounter.make)(_ addCube _).surfaceArea

      val outside = mutable.Set[Cube]()
      val initial = Cube(minX - 1, minY - 1, minZ - 1)
      reachable(outside)(initial)
      val pockets =
        for
          x <- minX to maxX
          y <- minX to maxY
          z <- minX to maxZ
          c = Cube(x, y, z)
          if !cubes(c) && !outside(c)
        yield c

      calculateSurfaceArea(cubes) - calculateSurfaceArea(pockets.toSet)

  def part1(is: UStream[String]): Task[Int] =
    for areaCounter <- is
        .map(Cube.parse)
        .runFold(AreaCounter.make)(_ addCube _)
    yield areaCounter.surfaceArea

  def part2(is: UStream[String]): Task[Int] =
    is.map(Cube.parse)
      .runCollect
      .map(_.toSet)
      .map(UnreachableCounter.apply)
      .map(_.surfaceAreaWithoutPockets)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
