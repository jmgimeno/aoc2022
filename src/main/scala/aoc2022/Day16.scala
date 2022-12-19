package aoc2022

import zio.*
import zio.stream.*

import scala.annotation.*
import scala.collection.mutable
import scala.math.Ordering.String
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

  case class MaxFlowFinder1(scan: ScanOutput):

    def findMaxFlow1(maxTime: Int): Int =

      case class State1(
          valve: String,
          time: Int,
          opened: Set[String] = Set.empty,
          accum: Int = 0
      ):
        def branch: List[State1] =
          openValve ++ move
        private def openValve: List[State1] =
          if scan.rate(valve) > 0 && !opened(valve)
          then List(copy(time = time + 1, opened = opened + valve, accum = accum + releasing))
          else List()
        private def move: List[State1] =
          scan.output(valve).map { next =>
            copy(valve = next, time = time + 1, accum = accum + releasing)
          }
        private def releasing =
          opened.map(scan.rate).sum
        val lowerBound: Int =
          accum + (maxTime - time) * releasing
        private def bestImprovement: Int =
          val closed = (scan.valves.keySet -- opened).map(scan.rate).toList.sortBy(v => -v)
          val times = time + 1 to maxTime by 2
          closed.zip(times).map((r, t) => r * (maxTime - t)).sum
        val upperBound: Int =
          lowerBound + bestImprovement
        def allOpened: Boolean =
          opened.size == scan.openableValves
        def isSolution: Boolean =
          time == maxTime
      end State1

      given Ordering[State1] with
        def compare(left: State1, right: State1) =
          left.upperBound.compare(right.upperBound)

      val open = mutable.PriorityQueue(State1("AA", 0))

      @tailrec def loop(
          maxSoFar: Int = Integer.MIN_VALUE
      ): Int =
        if open.isEmpty then maxSoFar
        else
          val current = open.dequeue()
          val lowerBoundOrSolution = math.max(current.lowerBound, maxSoFar)
          if current.isSolution || current.allOpened then
            loop(math.max(lowerBoundOrSolution, maxSoFar))
          else
            val branches =
              current.branch.filter(_.upperBound > lowerBoundOrSolution)
            open.enqueue(branches*)
            loop(lowerBoundOrSolution)
      loop()
  end MaxFlowFinder1

  case class MaxFlowFinder2(scan: ScanOutput):

    def findMaxFlow2(maxTime: Int): Int =

      case class State2(
          valve1: String,
          valve2: String,
          time: Int,
          opened: Set[String] = Set.empty,
          accum: Int = 0
      ):
        def branch =
          bothOpen ++ oneOpensTwoMoves ++ twoOpensOneMove ++ bothMove

        private def bothOpen =
          if valve1 != valve2
            && scan.rate(valve1) > 0 && !opened(valve1)
            && scan.rate(valve2) > 0 && !opened(valve2)
          then
            List(
              copy(time = time + 1, opened = opened + valve1 + valve2, accum = accum + releasing)
            )
          else List()

        private def oneOpensTwoMoves =
          if scan.rate(valve1) > 0 && !opened(valve1) then
            scan.output(valve2).map { nextValve2 =>
              copy(
                valve1 = String.min(valve1, nextValve2),
                valve2 = String.max(valve1, nextValve2),
                time = time + 1,
                opened = opened + valve1,
                accum = accum + releasing
              )
            }
          else List()

        private def twoOpensOneMove =
          if scan.rate(valve2) > 0 && !opened(valve2) then
            scan.output(valve1).map { nextValve1 =>
              copy(
                valve1 = String.min(nextValve1, valve2),
                valve2 = String.max(nextValve1, valve2),
                time = time + 1,
                opened = opened + valve2,
                accum = accum + releasing
              )
            }
          else List()

        private def bothMove =
          val pairs =
            for
              nextValve1 <- scan.output(valve1)
              nextValve2 <- scan.output(valve2)
              minValve = String.min(nextValve1, nextValve2)
              maxValve = String.max(nextValve1, nextValve2)
            yield (minValve, maxValve)
          pairs.toSet.map { case (nextValve1, nextValve2) =>
            copy(
              valve1 = nextValve1,
              valve2 = nextValve2,
              time = time + 1,
              accum = accum + releasing
            )
          }

        private def releasing =
          opened.map(scan.rate).sum
        val lowerBound: Int =
          accum + (maxTime - time) * releasing
        private def bestImprovement: Int =
          val closed = (scan.valves.keySet -- opened).map(scan.rate).toList.sortBy(v => -v)
          val paired = closed.sliding(2, 2).map(_.sum) // Why this does not work?
          val times = time + 1 to maxTime by 2
          closed.zip(times).map((r, t) => r * (maxTime - t)).sum
        val upperBound: Int =
          lowerBound + bestImprovement
        def allOpened: Boolean =
          opened.size == scan.openableValves
        def isSolution: Boolean =
          time == maxTime
      end State2

      given Ordering[State2] with
        def compare(left: State2, right: State2) =
          left.upperBound.compare(right.upperBound)

      val open = mutable.PriorityQueue(State2("AA", "AA", 0))

      @tailrec def loop(
          maxSoFar: Int = Integer.MIN_VALUE
      ): Int =
        if open.isEmpty then maxSoFar
        else
          val current = open.dequeue()
          val State2(me, elephant, time, opened, _) = current
          val lowerBoundOrSolution = math.max(current.lowerBound, maxSoFar)
          if time == maxTime || current.allOpened then
            loop(math.max(lowerBoundOrSolution, maxSoFar))
          else
            val branches =
              current.branch.filter(_.upperBound > lowerBoundOrSolution)
            open.enqueue(branches*)
            loop(lowerBoundOrSolution)
      loop()
  end MaxFlowFinder2

  lazy val inputStream =
    ZStream
      .fromFileName("data/input16.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for scan <- is.map(Valve.parse).runCollect.map(ScanOutput.apply)
    yield MaxFlowFinder1(scan).findMaxFlow1(30)

  def part2(is: UStream[String]): Task[Int] =
    for scan <- is.map(Valve.parse).runCollect.map(ScanOutput.apply)
    yield MaxFlowFinder2(scan).findMaxFlow2(26)

  lazy val run =
    part1(inputStream).debug("PART1").zipPar(part2(inputStream).debug("PART2"))
