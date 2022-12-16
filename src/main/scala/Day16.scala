import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day16 extends ZIOAppDefault:

  case class ScanOutput private (
      startingValve: String,
      valves: Map[String, Valve]
  ):
    def rate(valve: String): Int =
      valves(valve).rate

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
        case s"Valve $name has flow rate=$rate; tunnels lead to valves $output" =>
          Valve(name, rate.toInt, output.split(", ").toList)

  case class MaxFlowFinder(scan: ScanOutput):
    def findMaxFlow(maxMinutes: Int): Option[Int] =
      case class State(
          valve: String,
          flow: Int,
          minute: Int,
          opened: Set[String]
      ):
        def isFinal = minute > maxMinutes
        def enter =
          copy(
            flow =
              if opened(valve) then flow
              else flow + (maxMinutes - minute) * scan.rate(valve),
            minute = minute + 1,
            opened = opened + valve
          )
        def next = ???

      end State

      val initial = State(scan.startingValve, 0, 0, Set.empty)
      val fringe = Vector(initial)

      def loop(fringe: Vector[State], time: Int): Option[State] =
        fringe match
          case current +: rest =>
            if current.isFinal then Some(current)
            else ???
          case _ => None

      loop(fringe, 0).map(_.flow)

    end findMaxFlow
  end MaxFlowFinder

  lazy val inputStream =
    ZStream
      .fromFileName("data/input16.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for scan <- is.map(Valve.parse).runCollect.map(ScanOutput.apply)
    yield MaxFlowFinder(scan).findMaxFlow(20).get

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
