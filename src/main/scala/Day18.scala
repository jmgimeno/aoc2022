import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day18 extends ZIOAppDefault:

  case class Cube(x: Int, y: Int, z: Int):
    def contacts(other: Cube) =
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

  case class AreaCounter private (counters: Map[Cube, Int]):
    def surfaceArea = counters.values.sum
    def addCube(cube: Cube): AreaCounter =
      AreaCounter {
        counters
          .foldLeft(counters.updated(cube, 6)) { case (counters, (other, counter)) =>
            if other.contacts(cube) then
              counters
                .updated(other, counter - 1)
                .updated(cube, counters(cube) - 1)
            else counters
          }
      }

  object AreaCounter:
    def make = AreaCounter(Map.empty)

  def part1(is: UStream[String]): Task[Int] =
    is.map(Cube.parse).runFold(AreaCounter.make)(_ addCube _).map(_.surfaceArea)

  def part2(is: UStream[String]): Task[Int] =
    ???

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
