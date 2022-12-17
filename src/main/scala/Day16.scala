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
      valve: String,
      time: Int,
      opened: Set[String] = Set.empty,
      accum: Int = 0
  ):
    def branch(scan: ScanOutput): List[State] =
      openValve(scan) ++ move(scan)

    private def openValve(scan: ScanOutput): List[State] =
      if scan.rate(valve) > 0 && !opened(valve)
      then List(copy(time = time + 1, opened = opened + valve, accum = accum + releasing(scan)))
      else List()
    private def move(scan: ScanOutput): List[State] =
      scan.output(valve).map { next =>
        copy(valve = next, time = time + 1, accum = accum + releasing(scan))
      }
    def releasing(scan: ScanOutput) =
      opened.map(scan.rate).sum
    def upperBound(scan: ScanOutput, maxTime: Int): Int =
      // not very good bound but works on example
      accum + (maxTime - time + 1) * scan.valves.values.map(_.rate).sum
      // solves example but in input returns 2372 which id too hight!?
      val current = accum + (maxTime - time + 1) * releasing(scan)
      val closed = (scan.valves.keySet -- opened).map(scan.rate).toList.sortBy(v => -v)
      val times = time + 1 to maxTime by 2
      val bestFuture = closed.zip(times).map((r, t) => r * (maxTime - t)).sum
      current + bestFuture

  case class MaxFlowFinder(scan: ScanOutput):

    def findMaxFlow(maxTime: Int): Int =

      @tailrec def loop(open: List[State], maxSoFar: Int = 0): Int =
        open match
          case Nil => maxSoFar
          case current :: rest =>
            val State(valve, time, opened, accum) = current
            if time == maxTime then loop(rest, math.max(accum, maxSoFar))
            else
              val branches = current.branch(scan)
              val pruned = branches.filter(_.upperBound(scan, maxTime) > maxSoFar)
              loop(pruned ::: rest, maxSoFar)
      loop(List(State(scan.startingValve, 0)))

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
