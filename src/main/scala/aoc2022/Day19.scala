package aoc2022

import zio.*
import zio.stream.*

import math.Ordered.orderingToOrdered
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day19 extends ZIOAppDefault:

  enum Resource:
    case Ore, Clay, Obsidian, Geode

  case class Robot(makes: Resource, costs: Map[Resource, Int])

  case class Blueprint(id: Int, specifications: Map[Resource, Robot]):
    def cost(robotFor: Resource)(r: Resource) = specifications(robotFor).costs.getOrElse(r, 0)
    def max(resource: Resource) =
      specifications.values
        .map(_.costs.getOrElse(resource, 0))
        .max

  object Blueprint:
    import Resource.*
    def parse(line: String): Blueprint =
      val patternNum = raw"Blueprint (\d+):".r
      val patternOre = raw"Each ore robot costs (\d+) ore.".r
      val patternClay = raw"Each clay robot costs (\d+) ore.".r
      val patternObsidian = raw"Each obsidian robot costs (\d+) ore and (\d+) clay.".r
      val patternGeode = raw"Each geode robot costs (\d+) ore and (\d+) obsidian.".r
      val num = patternNum.findFirstMatchIn(line).get.group(1).toInt
      val costOreRobot = patternOre.findFirstMatchIn(line).get.group(1).toInt
      val costClayRobot = patternClay.findFirstMatchIn(line).get.group(1).toInt
      val costObsidianRobotOre = patternObsidian.findFirstMatchIn(line).get.group(1).toInt
      val costObsidianRobotClay = patternObsidian.findFirstMatchIn(line).get.group(2).toInt
      val costGeodeRobotOre = patternGeode.findFirstMatchIn(line).get.group(1).toInt
      val costGeodeRobotObsidian = patternGeode.findFirstMatchIn(line).get.group(2).toInt
      Blueprint(
        num,
        Map(
          Ore -> Robot(Ore, Map(Ore -> costOreRobot)),
          Clay -> Robot(Clay, Map(Ore -> costClayRobot)),
          Obsidian -> Robot(
            Obsidian,
            Map(Ore -> costObsidianRobotOre, Clay -> costObsidianRobotClay)
          ),
          Geode -> Robot(Geode, Map(Ore -> costGeodeRobotOre, Obsidian -> costGeodeRobotObsidian))
        )
      )

  case class State(
      time: Int,
      blueprint: Blueprint,
      robots: Map[Resource, Int],
      resources: Map[Resource, Int]
  ):
    def canCreateRobotFor(resource: Resource) =
      Resource.values.forall(r => blueprint.cost(resource)(r) <= resources(r)) &&
        (resource == Resource.Geode || robots(resource) < blueprint.max(resource))

    def noCreation =
      copy(
        time = time + 1,
        resources = resources.map((r, qty) => r -> (qty + robots(r)))
      )

    def createOneRobot =
      Resource.values
        .filter(canCreateRobotFor)
        .map(newRobotFor =>
          copy(
            time = time + 1,
            resources =
              resources.map((r, qty) => r -> (qty + robots(r) - blueprint.cost(newRobotFor)(r))),
            robots = robots.updated(newRobotFor, robots(newRobotFor) + 1)
          )
        )
        .toList

    def next: List[State] = noCreation :: createOneRobot

  object State:
    given Ordering[State] =
      Ordering
        .by[State, Int](s => s.robots(Resource.Geode))
        .orElseBy[Int](s => s.resources(Resource.Geode))
        .orElseBy[Int](s => s.robots(Resource.Obsidian))
        .orElseBy[Int](s => s.resources(Resource.Obsidian))
        .orElseBy[Int](s => s.robots(Resource.Clay))
        .orElseBy[Int](s => s.resources(Resource.Clay))
        .orElseBy[Int](s => s.robots(Resource.Ore))
        .orElseBy[Int](s => s.resources(Resource.Ore))

  class Simulation(blueprint: Blueprint, maxTime: Int):
    def upperBound(state: State) =
      val restTime = maxTime - state.time
      state.resources(Resource.Geode) +
        restTime * state.robots(Resource.Geode) +
        (restTime * restTime - 2 * restTime + 2) / 2
    def qualityLevel: Int = blueprint.id * maxGeodes
    def maxGeodes: Int =
      val robots = Resource.values.map(r => (r, if r == Resource.Ore then 1 else 0)).toMap
      val resources = Resource.values.map(_ -> 0).toMap
      val initial = State(0, blueprint, robots, resources)
      var best = initial
      val fringe = mutable.PriorityQueue(initial)
      val visited = mutable.Set.empty[State]
      while !fringe.isEmpty do
        val current = fringe.dequeue()
        visited += current
        if upperBound(current) >= best.resources(Resource.Geode) then
          if current.time == maxTime
          then
            if current.resources(Resource.Geode) > best.resources(Resource.Geode) then
              best = current
          else fringe.enqueue(current.next.filterNot(visited)*)
      best.resources(Resource.Geode)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input19.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    is.map(Blueprint.parse).map(Simulation(_, 24).qualityLevel).runSum.map(_.toInt)

  def part2(is: UStream[String]): Task[Int] =
    is.take(3).map(Blueprint.parse).map(Simulation(_, 32).maxGeodes).runFold(1)(_ * _)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
