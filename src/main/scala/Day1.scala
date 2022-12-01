import zio.*
import zio.stream.*

import scala.io.Source
import java.io.FileReader

object Day1 extends ZIOAppDefault:

  // with files

  def readFile(name: String): List[String] =
    Source.fromFile("data/day1.txt").getLines.toList

  def chunk(lines: List[String]): List[List[String]] =
    if lines.isEmpty then List.empty
    else
      val (group, rest) = lines.span(_.nonEmpty)
      group :: chunk(rest.drop(1))

  def part1(lines: List[String]): Int =
    chunk(lines).map(g => g.map(_.toInt).sum).max

  def part2(lines: List[String]): Int =
    chunk(lines).map(g => g.map(_.toInt).sum).sorted.takeRight(3).sum

  // with streams

  val sumsStream =
    ZStream
      .fromFileName("data/day1.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .split(_.isEmpty)
      .map(chunk => chunk.map(_.toInt))
      .map(_.sum)

  val part1Result =
    sumsStream
      .runFold(0)(_ max _)

  val part2Result =
    sumsStream
      .runFold[List[Int]](List.empty) { (max3, s) =>
        (s :: max3).sorted.takeRight(3)
      }
      .map(_.sum)

  val run =
    part1Result.debug *> part2Result.debug

  // val run = for
  //   lines <- ZIO.attempt(readFile("data/day1.txt"))
  //   p1 = part1(lines)
  //   _ <- Console.printLine(s"part1 = $p1")
  //   p2 = part2(lines)
  //   _ <- Console.printLine(s"part2 = $p2")
  // yield ()
