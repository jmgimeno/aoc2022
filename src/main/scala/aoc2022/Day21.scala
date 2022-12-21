package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec

object Day21 extends ZIOAppDefault:

  enum Job:
    case Num(n: Long)
    case Sum(left: String, right: String)
    case Dif(left: String, right: String)
    case Mul(left: String, right: String)
    case Div(left: String, right: String)

  object Job:
    def parse(line: String): (String, Job) = line match
      case s"$monkey: $left + $right" => (monkey, Sum(left, right))
      case s"$monkey: $left - $right" => (monkey, Dif(left, right))
      case s"$monkey: $left * $right" => (monkey, Mul(left, right))
      case s"$monkey: $left / $right" => (monkey, Div(left, right))
      case s"$monkey: $num"           => (monkey, Num(num.toLong))

  class Evaluator(jobs: Map[String, Job]):
    def yell(monkey: String): Long =
      val environment = jobs.collect { case (monkey, Job.Num(num)) => (monkey, num) }.toMap
      val stack = List(monkey)
      @tailrec def loop(stack: List[String], environment: Map[String, Long]): Long =
        if environment.contains(monkey) then environment(monkey)
        else
          stack match
            case head :: next =>
              if environment.contains(head) then loop(next, environment)
              else
                jobs(head) match
                  case Job.Num(num) => loop(next, environment.updated(head, num))
                  case Job.Sum(left, right) =>
                    if environment.contains(left) && environment.contains(right) then
                      loop(next, environment.updated(head, environment(left) + environment(right)))
                    else loop(left :: right :: stack, environment)
                  case Job.Dif(left, right) =>
                    if environment.contains(left) && environment.contains(right) then
                      loop(next, environment.updated(head, environment(left) - environment(right)))
                    else loop(left :: right :: stack, environment)
                  case Job.Mul(left, right) =>
                    if environment.contains(left) && environment.contains(right) then
                      loop(next, environment.updated(head, environment(left) * environment(right)))
                    else loop(left :: right :: stack, environment)
                  case Job.Div(left, right) =>
                    if environment.contains(left) && environment.contains(right) then
                      loop(next, environment.updated(head, environment(left) / environment(right)))
                    else loop(left :: right :: stack, environment)
            case Nil => assert(false, "should never happen")
      loop(stack, environment)

  class Solver(jobs: Map[String, Job]):
    def monkeys(start: String, found: Set[String] = Set.empty): Set[String] =
      if found.contains(start) then found
      else
        jobs(start) match
          case Job.Num(n)           => found + start
          case Job.Sum(left, right) => monkeys(left, monkeys(right, found + start))
          case Job.Dif(left, right) => monkeys(left, monkeys(right, found + start))
          case Job.Mul(left, right) => monkeys(left, monkeys(right, found + start))
          case Job.Div(left, right) => monkeys(left, monkeys(right, found + start))

    def solve(root: String, human: String): Long =
      val Job.Sum(left, right) = jobs(root).asInstanceOf[Job.Sum] // FIXME!!
      assert(monkeys(left)(human) && !monkeys(right)(human))
      val evaluator = Evaluator(jobs)
      val rightValue = evaluator.yell(right)
      solveFor(human, left, rightValue, evaluator)

    def solveFor(human: String, root: String, target: Long, evaluator: Evaluator): Long =
      val environment = jobs.collect {
        case (monkey, Job.Num(num)) if monkey != human => (monkey, num)
      }.toMap
      val stack = List(root)
      @tailrec def loop(stack: List[String], target: Long): Long =
        stack match
          case head :: next if head == human => target
          case head :: next =>
            jobs(head) match
              case Job.Num(n) =>
                loop(next, target)
              case Job.Sum(left, right) =>
                if monkeys(left)(human) then loop(left :: next, target - evaluator.yell(right))
                else loop(right :: next, target - evaluator.yell(left))
              case Job.Dif(left, right) =>
                if monkeys(left)(human) then loop(left :: next, target + evaluator.yell(right))
                else loop(right :: next, evaluator.yell(left) - target)
              case Job.Mul(left, right) =>
                if monkeys(left)(human) then loop(left :: next, target / evaluator.yell(right))
                else loop(right :: next, target / evaluator.yell(left))
              case Job.Div(left, right) =>
                if monkeys(left)(human) then loop(left :: next, target * evaluator.yell(right))
                else loop(right :: next, evaluator.yell(left) / target)
          case Nil => assert(false, "should't happen")

      loop(stack, target)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input21.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Long] =
    for parsedLines <- is.map(Job.parse).runCollect.map(_.toMap)
    yield Evaluator(parsedLines).yell("root")

  def part2(is: UStream[String]): Task[Long] =
    for parsedLines <- is.map(Job.parse).runCollect.map(_.toMap)
    yield Solver(parsedLines).solve("root", "humn")

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
