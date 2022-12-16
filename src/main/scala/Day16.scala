import zio.*
import zio.stream.*

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

  case class MaxFlowFinder(scan: ScanOutput):
    def findMaxFlow(maxMinutes: Int): Int =
      case class State(
          current: String,
          minute: Int = 0,
          flow: Int = 0,
          opened: Set[String] = Set.empty,
          path: List[String] = List.empty
      ):
        def nexts: List[State] =
          scan
            .output(current)
            .map { nextValve =>
              copy(
                current = nextValve,
                minute = minute + 1,
                path = nextValve :: path
              )
            }
            .filter { state =>
              state.opened.size < scan.openableValves
            }
      end State

      // calc result flow when no more valves can be opened
      // calc maxflow atainable opening all remainnin valves
      // purge if expected flo is lower than best to far
      def loop(
          fringe: Vector[State],
          bestSoFar: Int
      ): Int =
        fringe match
          case current +: rest =>
            println(s"minute: ${current.minute} best $bestSoFar")
            if current.minute > maxMinutes then bestSoFar
            else
              val State(name, minute, flow, opened, path) = current
              val newFlow = flow + opened.map(scan.rate).sum
              val newOpened =
                if (scan.rate(name) != 0) then opened + name else opened
              val nextBest = math.max(newFlow, bestSoFar)
              val nexts = current
                .copy(
                  opened = newOpened,
                  flow = newFlow
                )
                .nexts
              loop(rest ++ nexts, nextBest)
          case _ => assert(false, "havent't found")
      loop(Vector(State(scan.startingValve)), 1)
    end findMaxFlow
  end MaxFlowFinder

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
