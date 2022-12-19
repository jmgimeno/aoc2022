package aoc2022

import zio.*
import zio.stream.*

import math.Ordered.orderingToOrdered
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day19 extends ZIOAppDefault:

  enum Resource:
    case Ore, Clay, Obsidian, Geode

  case class Robot(makes: Resource, costs: Map[Resource, Int]):
    def cost(r: Resource) = costs.getOrElse(r, 0)

  enum RobotSpec:
    case Ore(ore: Int)
    case Clay(ore: Int)
    case Obsidian(ore: Int, clay: Int)
    case Geode(ore: Int, obsidian: Int)

    def makeRobot: Robot = this match
      case Ore(ore) =>
        Robot(Resource.Ore, Map(Resource.Ore -> ore))
      case Clay(ore) =>
        Robot(Resource.Clay, Map(Resource.Ore -> ore))
      case Obsidian(ore, clay) =>
        Robot(Resource.Obsidian, Map(Resource.Ore -> ore, Resource.Clay -> clay))
      case Geode(ore, obsidian) =>
        Robot(Resource.Geode, Map(Resource.Ore -> ore, Resource.Obsidian -> obsidian))

  case class Blueprint(id: Int, specifications: Map[Resource, RobotSpec]):
    def makeRobot(resource: Resource) = specifications(resource).makeRobot

  object Blueprint:
    import RobotSpec.*
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
      val costGeodeRobotClay = patternGeode.findFirstMatchIn(line).get.group(2).toInt
      Blueprint(
        num,
        Map(
          Resource.Ore -> Ore(costOreRobot),
          Resource.Clay -> Clay(costClayRobot),
          Resource.Obsidian -> Obsidian(costObsidianRobotOre, costObsidianRobotClay),
          Resource.Geode -> Geode(costGeodeRobotOre, costGeodeRobotClay)
        )
      ).tap(println)

  case class State(
      time: Int,
      blueprint: Blueprint,
      robots: Map[Resource, Int],
      resources: Map[Resource, Int]
  ):
    def next: List[State] = ???

  object State:
    given Ordering[State] with
      def compare(left: State, right: State) =
        left.resources(Resource.Geode).compare(right.resources(Resource.Geode))

  class Simulation(blueprint: Blueprint, maxTime: Int):
    def qualityLevel: Int = blueprint.id * maxGeodes
    def maxGeodes: Int =
      val robots = Resource.values.map(r => (r, if r == Resource.Ore then 1 else 0)).toMap
      val resources = Resource.values.map(_ -> 0).toMap
      val initial = State(0, blueprint, robots, resources)
      var best = initial
      val fringe = mutable.PriorityQueue(initial)
      while !fringe.isEmpty do
        val current = fringe.dequeue()
        if current.time == maxTime
        then
          if current > best then best = current
        else fringe.enqueue(current.next*)

      best.resources(Resource.Geode)

  lazy val inputStream =
    ZStream
      .fromFileName("data/input19.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    is.map(Blueprint.parse).map(Simulation(_, 24).qualityLevel).runSum.map(_.toInt)

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
