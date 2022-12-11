import zio.*
import zio.stream.*

import scala.annotation.tailrec
import scala.collection.mutable

import scala.util.chaining.scalaUtilChainingOps

object Day11 extends ZIOAppDefault:

  type WorryLevel = BigInt
  type MonkeyId = Int

  enum Operation:
    case Times(n: WorryLevel)
    case Plus(n: WorryLevel)
    case Squared

    def apply(worryLevel: WorryLevel): WorryLevel = this match
      case Times(n) => worryLevel * n
      case Plus(n)  => worryLevel + n
      case Squared  => worryLevel * worryLevel

  enum Condition:
    case DivisibleBy(n: Int)

    def apply(worryLevel: WorryLevel): Boolean = this match
      case DivisibleBy(n) => worryLevel % n == 0

  case class Test(condition: Condition, ifTrue: MonkeyId, ifFalse: MonkeyId):
    def apply(worryLevel: WorryLevel): MonkeyId =
      if (condition(worryLevel)) then ifTrue else ifFalse

  case class MonkeyRule(
      op: Operation,
      test: Test
  )

  type Items = List[WorryLevel]

  object Parser:
    def parseMonkeyId(line: String): MonkeyId = line match
      case s"Monkey $monkeyId:" => monkeyId.toInt
    def parseStartingItems(line: String): List[WorryLevel] = line match
      case s"  Starting items: $items" =>
        items.split(", ").map(BigInt(_)).toList
    def parseOperation(line: String): Operation = line match
      case s"  Operation: new = old * old" => Operation.Squared
      case s"  Operation: new = old * $n"  => Operation.Times(n.toInt)
      case s"  Operation: new = old + $n"  => Operation.Plus(n.toInt)
    def parseCondition(line: String): Condition = line match
      case s"  Test: divisible by $n" => Condition.DivisibleBy(n.toInt)
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
  type Inspections = Map[MonkeyId, BigInt]
  type Rules = Map[MonkeyId, MonkeyRule]
  type Trace = mutable.Map[Int, Simulation]
  type InvTrace = mutable.Map[Inventory, Int]

  case class Simulation private (
      inventory: Inventory,
      inspections: Inspections,
      rules: Rules,
      factor: Int
  ):

    def run(rounds: Int): Simulation =
      (1 to rounds).foldLeft(this) { (sim, _) => sim.round }

    def round: Simulation =
      inventory.keys.toList.sorted.foldLeft(this)(_ round _)

    def round(monkeyId: MonkeyId): Simulation =
      inventory(monkeyId)
        .foldLeft(this) { (sim, item) =>
          val worryLevel = sim.rules(monkeyId).op(item) / factor
          val throwsTo = sim.rules(monkeyId).test(worryLevel)
          sim
            .copy(
              inventory = sim.inventory
                .updated(throwsTo, sim.inventory(throwsTo) :+ worryLevel)
                .updated(monkeyId, sim.inventory(monkeyId).tail),
              inspections =
                sim.inspections.updated(monkeyId, sim.inspections(monkeyId) + 1)
            )
        }

    def part1: BigInt =
      runWithTrace(20)._2.inspections.values.toList.sorted
        .takeRight(2)
        .foldLeft(BigInt(1))(_ * _)

    def runWithTrace(numRounds: Int): (Int, Simulation, Trace, InvTrace) =

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
          runWithTrace(round + 1, simulation.round)

      val (executedRounds, repeatedSym) = runWithTrace(0, this)
      return (executedRounds, repeatedSym, trace, invTrace)

    def part2 =
      val (rounds, lastSim, trace, invTrace) = runWithTrace(10000)
      ???

  object Simulation:
    def make(
        parsedInput: Chunk[(MonkeyId, Items, MonkeyRule)],
        factor: Int
    ): Simulation =
      val inventory = parsedInput.map((id, inv, _) => id -> inv).toMap
      val inspections = parsedInput.map((id, inv, _) => id -> BigInt(0)).toMap
      val rules = parsedInput.map((id, _, rule) => id -> rule).toMap
      Simulation(inventory, inspections, rules, factor)

  extension (inspections: Inspections)
    infix def +(other: Inspections): Inspections =
      inspections.map { case (k, v) => k -> (v + other(k)) }
    infix def -(other: Inspections): Inspections =
      inspections.map { case (k, v) => k -> (v - other(k)) }
    infix def *(n: Int): Inspections =
      inspections.map { case (k, v) => k -> (v * n) }
    def metric: BigInt =
      inspections.values.toList.sorted
        .takeRight(2)
        .foldLeft(BigInt(1))(_ * _)

  val inputStream =
    ZStream
      .fromFileName("data/input11.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[BigInt] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 3).part1

  def part2[A](is: UStream[String]): Task[A] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseMonkey)
          .runCollect
    yield Simulation.make(parsedInput, 1).part2.asInstanceOf[A]

  lazy val run = part1(inputStream).debug("PART1")
