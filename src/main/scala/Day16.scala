import zio.*
import zio.stream.*

import scala.annotation.*
import scala.util.chaining.scalaUtilChainingOps

object Day16 extends ZIOAppDefault:

  case class ScanOutput private (
      startingValve: String,
      valves: Map[String, Valve]
  ):
    val openableValves = valves.values.count(_.rate != 0)
    val maxFlow = valves.values.map(_.rate).sum

    def rate(valve: String) =
      valves(valve).rate
    def output(valve: String) =
      valves(valve).output

  object ScanOutput:
    def apply(chunk: Chunk[Valve]): ScanOutput =
      ScanOutput(
        chunk(0).name,
        chunk.map(v => (v.name, v)).toMap
      )

  case class Valve(name: String, rate: Int, output: List[String])

  object Valve:
    def parse(line: String) =
      line match
        case s"Valve $name has flow rate=$rate; tunnel leads to valve $output" =>
          Valve(name, rate.toInt, List(output))
        case s"Valve $name has flow rate=$rate; tunnels lead to valves $output" =>
          Valve(name, rate.toInt, output.split(", ").toList)

  enum Step:
    case GoTo(valve: String)
    case OpenValve(valve: String)

  case class State(
      step: Step,
      remainigTime: Int,
      opened: Set[String] = Set.empty,
      accumReleased: Int = 0
  ):
    def below(max: Int): Boolean = ???

  case class MaxFlowFinder(scan: ScanOutput):
    def findMaxFlow(maxMinutes: Int): Int =
      @tailrec def loop(open: List[State], maxSoFar: Int = 0): Int =
        open match
          case Nil => maxSoFar
          case current :: rest =>
            val State(step, remaining, opened, accum) = current
            if remaining == 0 || current.below(maxSoFar) then
              loop(rest, maxSoFar)
            else
              step match
                case Step.OpenValve(valve) => ???
                case Step.GoTo(valve)      => ???

      loop(List(State(Step.GoTo(scan.startingValve), maxMinutes)))

  lazy val inputStream =
    ZStream
      .fromFileName("data/input16.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for scan <- is.map(Valve.parse).runCollect.map(ScanOutput.apply)
    yield MaxFlowFinder(scan).findMaxFlow(20)

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
