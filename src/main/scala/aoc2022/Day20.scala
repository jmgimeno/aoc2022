package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day20 extends ZIOAppDefault:

  val decryptionKey = 811589153L

  extension (n: Long) infix def %%(d: Int) = java.lang.Math.floorMod(n, d).toInt

  extension (indexed: mutable.ArrayBuffer[(Long, Int)])
    def sumNumsAfterZero(deltas: Int*): Long =
      val posOfZero = indexed.indexWhere(_._1 == 0)
      deltas.map { delta =>
        indexed((posOfZero + delta) %% indexed.size)._1
      }.sum

  case class Shuffle(numbers: Chunk[Long]):
    val indexed = mutable.ArrayBuffer.from(numbers).zipWithIndex
    def mix(times: Int) =
      (1 to times).foreach { _ =>
        (0 until numbers.size).foreach { pos =>
          val elemPos = indexed.indexWhere(_._2 == pos)
          val forward = indexed(elemPos)._1
          indexed.insert((elemPos + forward) %% (numbers.size - 1), indexed.remove(elemPos))
        }
      }
      indexed

  lazy val inputStream =
    ZStream
      .fromFileName("data/input20.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Long] =
    for numbers <- is.map(_.toLong).runCollect
    yield Shuffle(numbers).mix(1).sumNumsAfterZero(1000, 2000, 3000)

  def part2(is: UStream[String]): Task[Long] =
    for numbers <- is.map(_.toLong * decryptionKey).runCollect
    yield Shuffle(numbers).mix(10).sumNumsAfterZero(1000, 2000, 3000)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
