package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day11 extends ZIOAppDefault:

  type WorryLevel = Long
  type MonkeyId = Int
  type Counter = Long

  enum Operation:
    case Times(n: WorryLevel)
    case Plus(n: WorryLevel)
    case Squared

    def apply(worryLevel: WorryLevel): WorryLevel = this match
      case Times(n) => worryLevel * n
      case Plus(n)  => worryLevel + n
      case Squared  => worryLevel * worryLevel

  case class Condition(module: Int):
    def apply(worryLevel: WorryLevel): Boolean =
      worryLevel % module == 0

  case class Rule(
      op: Operation,
      cond: Condition,
      ifTrue: MonkeyId,
      ifFalse: MonkeyId
  ):
    def apply(reducer: Int, simplifier: Int)(
        input: WorryLevel
    ): (WorryLevel, MonkeyId) =
      val output = (op(input) / reducer) % simplifier
      val target = if cond(output) then ifTrue else ifFalse
      (output, target)

  type Counters = Map[MonkeyId, Counter]
  type Items = List[WorryLevel]
  type Inventory = Map[MonkeyId, Items]
  type Rules = Map[MonkeyId, Rule]

  type ParsedMonkey = (MonkeyId, Items, Rule)

  object Parser:
    private def parseMonkeyId(line: String): MonkeyId = line match
      case s"Monkey $monkeyId:" => monkeyId.toInt
    private def parseStartingItems(line: String): Items = line match
      case s"  Starting items: $items" =>
        items.split(", ").map(_.toLong).toList
    private def parseOperation(line: String): Operation = line match
      case s"  Operation: new = old * old" => Operation.Squared
      case s"  Operation: new = old * $n"  => Operation.Times(n.toInt)
      case s"  Operation: new = old + $n"  => Operation.Plus(n.toInt)
    private def parseCondition(line: String): Condition = line match
      case s"  Test: divisible by $n" => Condition(n.toInt)
    private def parseIf(branch: String, line: String): MonkeyId = line match
      case s"    If $branch: throw to monkey $monkeyId" => monkeyId.toInt

    def parseBlock(block: Chunk[String]): ParsedMonkey =
      val monkeyId = Parser.parseMonkeyId(block(0))
      val startingItems = Parser.parseStartingItems(block(1))
      val operation = Parser.parseOperation(block(2))
      val condition = Parser.parseCondition(block(3))
      val ifTrue = Parser.parseIf("true", block(4))
      val ifFalse = Parser.parseIf("false", block(5))
      (monkeyId, startingItems, Rule(operation, condition, ifTrue, ifFalse))
  end Parser

  type ParsedInput = Chunk[ParsedMonkey]

  case class Simulation private (
      rules: Rules,
      inventory: Inventory,
      counters: Counters
  ):
    def run(numRounds: Int, reducer: Int): Simulation =
      val simplifier =
        this.rules.valuesIterator.map(_.cond.module).product // thanks @ac_olite
      (1 to numRounds).foldLeft(this) { (sim, round) =>
        val monkeysIds = sim.rules.keySet.toList.sorted
        monkeysIds.foldLeft(sim) { (sim, monkeyId) =>
          val items = sim.inventory(monkeyId)
          items
            .foldLeft(sim) { (sim, item) =>
              val (output, target) =
                sim.rules(monkeyId)(reducer, simplifier)(item)
              sim.copy(
                inventory = sim.inventory
                  .updated(target, sim.inventory(target) :+ output)
              )
            }
            .pipe { sim =>
              sim.copy(
                inventory = sim.inventory.updated(monkeyId, List.empty),
                counters = sim.counters
                  .updated(monkeyId, sim.counters(monkeyId) + items.size)
              )
            }
        }
      }

    def result: Counter =
      counters.values.toList.sorted.takeRight(2).product

  object Simulation:
    def apply(parsedInput: ParsedInput): Simulation =
      val rules = parsedInput.map((id, _, rule) => id -> rule).toMap
      val inventory = parsedInput.map((id, inv, _) => id -> inv).toMap
      val counters = parsedInput.map((id, inv, _) => id -> 0L).toMap
      Simulation(rules, inventory, counters)

  val inputStream =
    ZStream
      .fromFileName("data/input11.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseBlock)
          .runCollect
    yield Simulation(parsedInput).run(20, 3).result

  def part2(is: UStream[String]): Task[Long] =
    for parsedInput <-
        is.split(_.isEmpty())
          .map(Parser.parseBlock)
          .runCollect
    yield Simulation(parsedInput).run(10_000, 1).result

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
