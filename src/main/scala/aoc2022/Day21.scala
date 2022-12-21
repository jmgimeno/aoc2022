package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec

object Day21 extends ZIOAppDefault:

  enum Job:
    case Num(n: Long)
    case Op(
        left: String,
        right: String,
        op: (Long, Long) => Long,
        invL: (Long, Long) => Long,
        invR: (Long, Long) => Long
    )

  object Job:
    def parse(line: String): (String, Job) = line match
      case s"$monkey: $left + $right" =>
        (monkey, Op(left, right, _ + _, (t, r) => t - r, (t, l) => t - l))
      case s"$monkey: $left - $right" =>
        (monkey, Op(left, right, _ - _, (t, r) => t + r, (t, l) => l - t))
      case s"$monkey: $left * $right" =>
        (monkey, Op(left, right, _ * _, (t, r) => t / r, (t, l) => t / l))
      case s"$monkey: $left / $right" =>
        (monkey, Op(left, right, _ / _, (t, r) => t * r, (t, l) => l / t))
      case s"$monkey: $num" => (monkey, Num(num.toLong))

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
                  case Job.Op(left, right, op, _, _) =>
                    if environment.contains(left) && environment.contains(right) then
                      loop(
                        next,
                        environment.updated(head, op(environment(left), environment(right)))
                      )
                    else loop(left :: right :: stack, environment)

            case Nil => assert(false, "should never happen")
      loop(stack, environment)

  class Solver(jobs: Map[String, Job]):
    def monkeys(start: String, found: Set[String] = Set.empty): Set[String] =
      if found.contains(start) then found
      else
        jobs(start) match
          case Job.Num(n)                   => found + start
          case Job.Op(left, right, _, _, _) => monkeys(left, monkeys(right, found + start))

    def solve(root: String, human: String): Long =
      val Job.Op(left, right, _, _, _) = jobs(root).asInstanceOf[Job.Op] // FIXME!!
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
              case Job.Op(left, right, op, invL, invR) =>
                if monkeys(left)(human) then loop(left :: next, invL(target, evaluator.yell(right)))
                else loop(right :: next, invR(target, evaluator.yell(left)))
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
