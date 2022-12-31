package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day20 extends ZIOAppDefault:

  extension (n: Int) infix def %%(d: Int) = java.lang.Math.floorMod(n, d)

  case class Shuffle(numbers: Chunk[Int]):

    val indexed = numbers.toBuffer.zipWithIndex
    (0 until numbers.size).foreach { pos =>
      val elemPos = indexed.indexWhere(_._2 == pos)
      val forward = indexed(elemPos)._1
      indexed.insert((elemPos + forward) %% (numbers.size - 1), indexed.remove(elemPos))
    }

    def sumNumsAfterZero(deltas: Int*): Int =
      val posOfZero = indexed.indexWhere(_._1 == 0)
      deltas.map { delta =>
        indexed((posOfZero + delta) %% indexed.size)._1
      }.sum

  lazy val inputStream =
    ZStream
      .fromFileName("data/input20.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Int] =
    for numbers <- is.map(_.toInt).runCollect
    yield Shuffle(numbers).sumNumsAfterZero(1000, 2000, 3000)

  def part2(is: UStream[String]): Task[Int] =
    ZIO.succeed(-1)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
