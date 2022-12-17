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

  case class MaxFlowFinder1(scan: ScanOutput):

    def findMaxFlow1(maxTime: Int): Int =

      case class State1(
          valve: String,
          time: Int,
          opened: Set[String] = Set.empty,
          accum: Int = 0
      ):
        lazy val branch: List[State1] =
          openValve ++ move
        private def openValve: List[State1] =
          if scan.rate(valve) > 0 && !opened(valve)
          then List(copy(time = time + 1, opened = opened + valve, accum = accum + releasing))
          else List()
        private def move: List[State1] =
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
        val isSolution: Boolean =
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
              current.branch.filter(_.upperBound >= lowerBoundOrSolution)
            open.enqueue(branches*)
            loop(lowerBoundOrSolution)
      loop()
  end MaxFlowFinder1

  case class MaxFlowFinder2(scan: ScanOutput):

    def findMaxFlow2(maxTime: Int): Int =

      case class State2(
          me: String,
          elephant: String,
          time: Int,
          opened: Set[String] = Set.empty,
          accum: Int = 0
      ):
        lazy val branch: List[State2] =
          bothOpenValves ++ meOpensValveAndElefantMoves ++ meMovesAndElephantOpensValve ++ bothMove

        private def bothOpenValves: List[State2] =
          if me != elephant && scan.rate(me) > 0 && !opened(me) && scan.rate(
              elephant
            ) > 0 && !opened(elephant)
          then
            List(copy(time = time + 1, opened = opened + me + elephant, accum = accum + releasing))
          else List()

        private def meOpensValveAndElefantMoves: List[State2] =
          if scan.rate(me) > 0 && !opened(me) then
            scan.output(elephant).map { nextElephant =>
              copy(
                elephant = nextElephant,
                time = time + 1,
                opened = opened + me,
                accum = accum + releasing
              )
            }
          else List()

        private def meMovesAndElephantOpensValve: List[State2] =
          if scan.rate(elephant) > 0 && !opened(elephant) then
            scan.output(me).map { nextMe =>
              copy(
                me = nextMe,
                time = time + 1,
                opened = opened + elephant,
                accum = accum + releasing
              )
            }
          else List()

        private def bothMove: List[State2] =
          for
            nextElephant <- scan.output(elephant)
            nextMe <- scan.output(me)
          yield copy(
            me = nextMe,
            elephant = nextElephant,
            time = time + 1,
            accum = accum + releasing
          )

        private val releasing =
          opened.map(scan.rate).sum
        val lowerBound: Int =
          accum + (maxTime - time) * releasing
        private val bestImprovement: Int =
          val closed = (scan.valves.keySet -- opened).map(scan.rate).toList.sortBy(v => -v)
          val paired = closed.sliding(2, 2).map(_.sum)
          val times = time + 1 to maxTime by 2
          closed.zip(times).map((r, t) => r * (maxTime - t)).sum
        val upperBound: Int =
          lowerBound + bestImprovement
        val allOpened: Boolean =
          opened.size == scan.openableValves
        val isSolution: Boolean =
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
              current.branch.filter(_.upperBound >= lowerBoundOrSolution)
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
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
