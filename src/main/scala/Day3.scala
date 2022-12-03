import zio.*
import zio.stream.*

object Day3 extends ZIOAppDefault:

  extension (c: Char)
    def priority: Int =
      if c.isLower then c.toInt - 'a'.toInt + 1
      else c.toInt - 'A'.toInt + 27

  def priority(rucksack: String): Int =
    val first = rucksack.substring(0, rucksack.size / 2).toSet
    val second = rucksack.substring(rucksack.size / 2).toSet
    val common = first.intersect(second)
    common.head.priority

  def priority(r1: String, r2: String, r3: String): Int =
    r1.toSet.intersect(r2.toSet).intersect(r3.toSet).head.priority

  def priority(chunk: Chunk[String]): Int =
    priority(chunk(0), chunk(1), chunk(2))

  val part1 =
    ZStream
      .fromFileName("data/input3.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .map(priority)
      .runFold(0)(_ + _)

  val part2 =
    ZStream
      .fromFileName("data/input3.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .grouped(3)
      .map(priority)
      .runFold(0)(_ + _)

  val run = part1.debug *> part2.debug
