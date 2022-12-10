import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day10 extends ZIOAppDefault:

  enum Instruction:
    case Noop
    case Addx(v: Int)

    def cycles: Int = this match
      case Noop    => 1
      case Addx(_) => 2

  object Instruction:
    def parse(line: String): Instruction = line match
      case s"noop"    => Noop
      case s"addx $v" => Addx(v.toInt)

  case class CPU(x: Int, cycle: Int):
    def execute(instruction: Instruction): CPU = instruction match
      case Instruction.Noop    => CPU(x, cycle + instruction.cycles)
      case Instruction.Addx(v) => CPU(x + v, cycle + instruction.cycles)

  object CPU:
    def make: CPU = CPU(1, 0)

  extension (trace: Chunk[CPU])
    def during(cycle: Int): CPU =
      trace.findLast { case CPU(v, c) => c < cycle }.get

    def signalStrength(cycles: Int*): Seq[Int] =
      cycles.map { cycle =>
        val CPU(v, _) = trace.during(cycle)
        cycle * v
      }

  extension (pixels: List[Char])
    def toImage: String =
      pixels.sliding(40, 40).map(_.mkString).mkString("\n")

  case class CRT(sprite: Int, buffer: List[Char])

  object CRT:
    def draw(trace: Chunk[CPU]): String =
      (1 to 240)
        .foldLeft(List.empty[Char]) { case (buffer, cycle) =>
          val sprite = trace.during(cycle).x
          val column = (cycle - 1) % 40
          val pixel =
            if (sprite - 1 to sprite + 1).contains(column) then '#' else '.'
          pixel :: buffer
        }
        .reverse
        .toImage

  val inputStream =
    ZStream
      .fromFileName("data/input10.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  def part1[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    for trace <-
        is.map(Instruction.parse)
          .scan(CPU.make)(_ execute _)
          .runCollect
    yield trace.signalStrength(20, 60, 100, 140, 180, 220).sum

  def part2[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, String] =
    for trace <-
        is.map(Instruction.parse)
          .scan(CPU.make)(_ execute _)
          .runCollect
    yield CRT.draw(trace)

  val run =
    part1(inputStream).debug("PART1")
      *> Console.printLine("PART2").orDie
      *> part2(inputStream).debug
