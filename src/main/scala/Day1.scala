import zio.*
import scala.io.Source

object Day1 extends ZIOAppDefault:

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
    chunk(lines).map(g => g.map(_.toInt).sum).sortBy(n => -n).take(3).sum

  val run = for
    lines <- ZIO.attempt(readFile("data/day1.txt"))
    p1 = part1(lines)
    _ <- Console.printLine(s"part1 = $p1")
    p2 = part2(lines)
    _ <- Console.printLine(s"part2 = $p2")
  yield ()
