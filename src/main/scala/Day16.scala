import zio.*
import zio.stream.*

import scala.annotation.*
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
    def lowerBound(scan: ScanOutput, maxTime: Int): Int =
      accum + (maxTime - time) * releasing(scan)
    private def bestImprovement(scan: ScanOutput, maxTime: Int): Int =
      val closed = (scan.valves.keySet -- opened).map(scan.rate).toList.sortBy(v => -v)
      val times = time + 1 to maxTime by 2
      closed.zip(times).map((r, t) => r * (maxTime - t)).sum
    def upperBound(scan: ScanOutput, maxTime: Int): Int =
      lowerBound(scan, maxTime) + bestImprovement(scan, maxTime)
    def allOpened(scan: ScanOutput): Boolean =
      opened.size == scan.openableValves

  case class MaxFlowFinder(scan: ScanOutput):

    def findMaxFlow(maxTime: Int): Int =

      @tailrec def loop(
          open: List[State],
          maxBound: Int = Integer.MIN_VALUE,
          maxSoFar: Int = Integer.MIN_VALUE
      ): Int =
        open match
          case Nil => maxSoFar
          case current :: rest =>
            val State(valve, time, opened, accum) = current
            val currentBound = math.max(current.lowerBound(scan, maxTime), maxBound)
            if time == maxTime || current.allOpened(scan) then
              loop(rest, currentBound, math.max(currentBound, maxSoFar))
            else
              val branches =
                current.branch(scan).filter(_.upperBound(scan, maxTime) >= currentBound)
              loop(branches ::: rest, currentBound, maxSoFar)
      loop(List(State("AA", 0)))

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
