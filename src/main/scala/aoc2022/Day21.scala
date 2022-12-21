package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

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
      def loop(stack: List[String], environment: Map[String, Long]): Long =
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

  lazy val inputStream =
    ZStream
      .fromFileName("data/input21.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Long] =
    for parsedLines <- is.map(Job.parse).runCollect.map(_.toMap)
    yield Evaluator(parsedLines).yell("root")

  def part2(is: UStream[String]): Task[Long] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
