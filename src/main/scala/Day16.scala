import zio.*
import zio.stream.*

import scala.annotation.*
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day16 extends ZIOAppDefault:

  case class ScanOutput private (
      valves: Map[String, Valve]
  ):
    val openableValves = valves.values.count(_.rate != 0)

    def rate(valve: String) =
      valves(valve).rate
    def output(valve: String) =
      valves(valve).output

  object ScanOutput:
    def apply(chunk: Chunk[Valve]): ScanOutput =
      ScanOutput(
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

  case class MaxFlowFinder(scan: ScanOutput):

    def findMaxFlow(maxTime: Int): Int =

      case class State(
          valve: String,
          time: Int,
          opened: Set[String] = Set.empty,
          accum: Int = 0
      ):
        lazy val branch: List[State] =
          openValve ++ move
        private def openValve: List[State] =
          if scan.rate(valve) > 0 && !opened(valve)
          then List(copy(time = time + 1, opened = opened + valve, accum = accum + releasing))
          else List()
        private def move: List[State] =
          scan.output(valve).map { next =>
            copy(valve = next, time = time + 1, accum = accum + releasing)
          }
        private val releasing =
          opened.map(scan.rate).sum
        val lowerBound: Int =
          accum + (maxTime - time) * releasing
        private val bestImprovement: Int =
          val closed = (scan.valves.keySet -- opened).map(scan.rate).toList.sortBy(v => -v)
          val times = time + 1 to maxTime by 2
          closed.zip(times).map((r, t) => r * (maxTime - t)).sum
        val upperBound: Int =
          lowerBound + bestImprovement
        val allOpened: Boolean =
          opened.size == scan.openableValves
      end State

      given Ordering[State] with
        def compare(left: State, right: State) =
          left.upperBound.compare(right.upperBound)

      val open = mutable.PriorityQueue(State("AA", 0))

      @tailrec def loop(
          maxSoFar: Int = Integer.MIN_VALUE
      ): Int =
        if open.isEmpty then maxSoFar
        else
          val current = open.dequeue()
          val State(valve, time, opened, _) = current
          val lowerBoundOrSolution = math.max(current.lowerBound, maxSoFar)
          if time == maxTime || current.allOpened then
            loop(math.max(lowerBoundOrSolution, maxSoFar))
          else
            val branches =
              current.branch.filter(_.upperBound >= lowerBoundOrSolution)
            open.enqueue(branches*)
            loop(lowerBoundOrSolution)
      loop()

  lazy val inputStream =
    ZStream
      .fromFileName("data/input16.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for scan <- is.map(Valve.parse).runCollect.map(ScanOutput.apply)
    yield MaxFlowFinder(scan).findMaxFlow(30)

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
