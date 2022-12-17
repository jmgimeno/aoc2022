import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day17 extends ZIOAppDefault:

  lazy val inputStream =
    ZStream
      .fromFileName("data/input17.txt")
      .via(ZPipeline.utf8Decode)
      .flatMap(ZStream.fromIterable)
      .orDie

  def part1(is: UStream[Char]): Task[Int] =
    ZIO.succeed(-1)

  def part2(is: UStream[Char]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
