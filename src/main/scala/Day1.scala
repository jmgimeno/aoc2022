import zio.*
import zio.stream.*

import scala.io.Source
import java.io.FileReader

object Day1 extends ZIOAppDefault:

  val inputStream =
    ZStream
      .fromFileName("data/input1.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)

  def sumStream[R, E](lines: ZStream[R, E, String]): ZStream[R, E, Int] =
    lines
      .split(_.isEmpty)
      .map(chunk => chunk.map(_.toInt).sum)

  def part1[R, E](lines: ZStream[R, E, String]): ZIO[R, E, Int] =
    sumStream(lines)
      .runFold(0)(_ max _)

  def part2[R, E](lines: ZStream[R, E, String]): ZIO[R, E, Int] =
    sumStream(lines)
      .runFold[List[Int]](List.empty) { (max3, s) =>
        (s :: max3).sorted.takeRight(3)
      }
      .map(_.sum)

  val run =
    part1(inputStream).debug("PART1") *> part2(inputStream)
      .debug("PART2")
