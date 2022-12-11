import zio.*
import zio.stream.*

import scala.annotation.tailrec
import scala.collection.mutable

import scala.util.chaining.scalaUtilChainingOps

object Day11 extends ZIOAppDefault:

  type WorryLevel = Int
  type MonkeyId = Int

  enum Operation:
    case Times(n: WorryLevel)
    case Plus(n: WorryLevel)
    case Squared

    def apply(worryLevel: WorryLevel): WorryLevel = this match
      case Times(n) => worryLevel * n
      case Plus(n)  => worryLevel + n
      case Squared  => worryLevel * worryLevel

  case class Condition(n: Int):
    def apply(worryLevel: WorryLevel): Boolean =
      worryLevel % n == 0

  case class Test(condition: Condition, ifTrue: MonkeyId, ifFalse: MonkeyId):
    def apply(worryLevel: WorryLevel): MonkeyId =
      if (condition(worryLevel)) then ifTrue else ifFalse

  case class MonkeyRule(
      op: Operation,
      test: Test
  ):
    def apply(input: WorryLevel): (WorryLevel, MonkeyId) =
      val output = input % test.condition.n
      val monkeyId =
        if output == 0
        then test.ifTrue
        else test.ifFalse
      (input, monkeyId)

  type Items = List[WorryLevel]

  object Parser:
    def parseMonkeyId(line: String): MonkeyId = line match
      case s"Monkey $monkeyId:" => monkeyId.toInt
    def parseStartingItems(line: String): List[WorryLevel] = line match
      case s"  Starting items: $items" =>
        items.split(", ").map(_.toInt).toList
    def parseOperation(line: String): Operation = line match
      case s"  Operation: new = old * old" => Operation.Squared
      case s"  Operation: new = old * $n"  => Operation.Times(n.toInt)
      case s"  Operation: new = old + $n"  => Operation.Plus(n.toInt)
    def parseCondition(line: String): Condition = line match
      case s"  Test: divisible by $n" => Condition(n.toInt)
    def parseIf(branch: String, line: String): MonkeyId = line match
      case s"    If $branch: throw to monkey $monkeyId" => monkeyId.toInt

    def parseMonkey(block: Chunk[String]): (MonkeyId, Items, MonkeyRule) =
      val monkeyId = Parser.parseMonkeyId(block(0))
      val startingItems = Parser.parseStartingItems(block(1))
      val operation = Parser.parseOperation(block(2))
      val condition = Parser.parseCondition(block(3))
      val ifTrue = Parser.parseIf("true", block(4))
      val ifFalse = Parser.parseIf("false", block(5))
      val test = Test(condition, ifTrue, ifFalse)
      (monkeyId, startingItems, MonkeyRule(operation, test))

  type Inventory = Map[MonkeyId, Items]
  type Inspections = Map[MonkeyId, Int]
  type Rules = Map[MonkeyId, MonkeyRule]
  type Trace = mutable.Map[Int, Simulation]
  type InvTrace = mutable.Map[Inventory, Int]

  case class Simulation private (
      inventory: Inventory,
      inspections: Inspections,
      rules: Rules,
      factor: Int
  ):

    def run(rounds: Int): Inspections =
      (1 to rounds)
        .foldLeft(this) { (sim, round) =>
          val nextSym = sim.round
          // println(s"round $round: ${nextSym.inspections}")
          nextSym
        }
        .inspections

    def round: Simulation =
      inventory.keys.toList.sorted.foldLeft(this)(_ round _)

    def round(monkeyId: MonkeyId): Simulation =
      inventory(monkeyId)
        .foldLeft(this) { (sim, item) =>
          val input = sim.rules(monkeyId).op(item) / factor
          val (worryLevel, throwsTo) = sim.rules(monkeyId)(input)
          // println(s"monkey $monkeyId throws $worryLevel to $throwsTo")
          sim
            .copy(
              inventory = sim.inventory
                .updated(throwsTo, sim.inventory(throwsTo) :+ worryLevel)
                .updated(monkeyId, sim.inventory(monkeyId).tail),
              inspections =
                sim.inspections.updated(monkeyId, sim.inspections(monkeyId) + 1)
            )
        }

    def runSimplifying(numRounds: Int): Inspections =
      (1 to numRounds)
        .foldLeft(this) { (sim, round) =>
          // println(s"$round / $numRounds")
          val nextSym = sim.roundSimplifying
          // println(s"round $round: ${nextSym.inspections}")
          nextSym
        }
        .inspections

    def roundSimplifying: Simulation =
      inventory.keys.toList.sorted.foldLeft(this)(_ roundSimplifying _)

    def roundSimplifying(monkeyId: MonkeyId): Simulation =
      inventory(monkeyId)
        .foldLeft(this) { (sim, item) =>
          val input = sim.rules(monkeyId).op(item) / factor
          val (worryLevel, throwsTo) = sim.rules(monkeyId)(input)
          // println(s"monkey $monkeyId throws $worryLevel to $throwsTo")
          sim
            .copy(
              inventory = sim.inventory
                .updated(throwsTo, sim.inventory(throwsTo) :+ worryLevel)
                .updated(monkeyId, sim.inventory(monkeyId).tail),
              inspections =
                sim.inspections.updated(monkeyId, sim.inspections(monkeyId) + 1)
            )
        }

    def runWithTrace(numRounds: Int): Inspections =

      val trace = mutable.Map.empty[Int, Simulation]
      val invTrace = mutable.Map.empty[Inventory, Int]

      @tailrec def runWithTrace(
          round: Int,
          simulation: Simulation
      ): (Int, Simulation) =
        if round >= numRounds || invTrace.contains(simulation.inventory)
        then (round, simulation)
        else
          trace += round -> simulation
          invTrace += simulation.inventory -> round
          runWithTrace(round + 1, simulation.roundSimplifying)

      val (executedRounds, lastSim) = runWithTrace(0, this)
      if executedRounds == numRounds
      then lastSim.inspections
      else inferSimulation(numRounds, executedRounds, lastSim, trace, invTrace)

    def inferSimulation(
        numRounds: Int,
        lastSeen: Int,
        lastSym: Simulation,
        trace: Trace,
        invTrace: InvTrace
    ): Inspections =
      println(s"lastSeen $lastSeen")
      println(lastSym)
      val firstSeen = invTrace(lastSym.inventory)
      println(s"firstSeen $firstSeen")
      val firstSym = trace(firstSeen)
      println(firstSym)
      val period = lastSeen - firstSeen
      println(s"period $period")
      val delta = lastSym.inspections - firstSym.inspections
      println(delta)
      val remaining = numRounds - lastSeen
      val cycles = remaining / period
      val extra = remaining % period
      println(
        s"remaining $remaining totalCycles $cycles extraSteps $extra"
      )
      val remainingCycles = delta * cycles
      print(remainingCycles)
      val extraSteps =
        trace(firstSeen + extra).inspections - firstSym.inspections
      println(extraSteps)
      val result =
        lastSym.inspections + remainingCycles + extraSteps
      println(result)
      result

  object Simulation:
    def make(
        parsedInput: Chunk[(MonkeyId, Items, MonkeyRule)],
        factor: Int
    ): Simulation =
      val inventory = parsedInput.map((id, inv, _) => id -> inv).toMap
      val inspections = parsedInput.map((id, inv, _) => id -> 0).toMap
      val rules = parsedInput.map((id, _, rule) => id -> rule).toMap
      Simulation(inventory, inspections, rules, factor)

  extension (inspections: Inspections)
    infix def +(other: Inspections): Inspections =
      inspections.map { case (k, v) => k -> (v + other(k)) }
    infix def -(other: Inspections): Inspections =
      inspections.map { case (k, v) => k -> (v - other(k)) }
    infix def *(n: Int): Inspections =
      inspections.map { case (k, v) => k -> (v * n) }
    def longMetric: Long =
      inspections.values.toList.sorted
        .takeRight(2)
        .foldLeft(1L)(_ * _)

  val inputStream =
    ZStream
      .fromFileName("data/input11.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 3).run(20).longMetric

  def part1Simplifying(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 3).runSimplifying(20).longMetric

  def part1SimplifyingWithTrace(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 3).runWithTrace(20).longMetric

  def part2(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 1).run(10_000).longMetric

  def part2Simplifying(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 1).runSimplifying(10_000).longMetric

  def part2SimplifyingWithTrace(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 1).runWithTrace(10_000).longMetric

  lazy val run =
    part1(inputStream).debug("PART1")
      *> part1Simplifying(inputStream).debug("PART1-SimpliFying")
      *> part1SimplifyingWithTrace(inputStream).debug(
        "PART1-Simplifying-WithTrace"
      )
      *> part2(inputStream).debug("PART2")
      *> part2Simplifying(inputStream).debug("PART2-Simplifying")
      *> part2SimplifyingWithTrace(inputStream).debug(
        "PART2-Simplifying-WithTrace"
      )
