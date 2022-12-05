import zio.*
import zio.stream.*

object Day5 extends ZIOAppDefault:

  type Stack = Vector[Char]
  type Level = Seq[(Char, Int)]

  final case class Stacks private (stacks: Array[Stack]):
    def execute(moves: Chunk[Move]): Unit =
      moves.foreach { case Move(n, from, to) =>
        (1 to n).foreach { _ =>
          val top = stacks(from - 1).last
          stacks(from - 1) = stacks(from - 1).dropRight(1)
          stacks(to - 1) = stacks(to - 1).appended(top)
        }
      }
    def execute2(moves: Chunk[Move]): Unit =
      moves.foreach { case Move(n, from, to) =>
        val top = stacks(from - 1).takeRight(n)
        stacks(from - 1) = stacks(from - 1).dropRight(n)
        stacks(to - 1) = stacks(to - 1) ++ top
      }

    def tops: String =
      stacks.map(_.last).mkString

  object Stacks:
    def apply(numStacks: Int, levels: Chunk[Level]): Stacks =
      val array = Array.fill(numStacks)(Vector.empty[Char])
      levels.foreach(level =>
        level.foreach((c, s) => array(s - 1) = array(s - 1).prepended(c))
      )
      Stacks(array)

  final case class Move(n: Int, from: Int, to: Int)

  def parseStackLevel(numStacks: Int)(line: String): Level =
    val positions = (0 until numStacks).map(i => i * 4 + 1)
    val crates = positions.map(line.charAt(_))
    crates.zipWithIndex.filter(_._1 != ' ').map((c, i) => (c, i + 1))

  def parseStacks(lines: Chunk[String], numStacks: Int): Stacks =
    val levels = lines.dropRight(1).map(parseStackLevel(numStacks))
    Stacks(numStacks, levels)

  def parseMove(line: String): Move =
    val movePattern = """^move (\d+) from (\d+) to (\d+)$""".r
    val m = movePattern.findAllIn(line)
    Move(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt)

  def parseMoves(lines: Chunk[String]): Chunk[Move] =
    lines.map(parseMove)

  val inputStream =
    ZStream
      .fromFileName("data/input5.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)

  def part1[R, E](
      is: ZStream[R, E, String],
      numStacks: Int = 9
  ): ZIO[R, E, String] =
    for
      parts <- is.split(_.isEmpty()).runCollect
      stacks = parseStacks(parts(0), numStacks)
      moves = parseMoves(parts(1))
      _ = stacks.execute(moves)
    yield stacks.tops

  def part2[R, E](
      is: ZStream[R, E, String],
      numStacks: Int = 9
  ): ZIO[R, E, String] =
    for
      parts <- is.split(_.isEmpty()).runCollect
      stacks = parseStacks(parts(0), numStacks)
      moves = parseMoves(parts(1))
      _ = stacks.execute2(moves)
    yield stacks.tops

  lazy val run = part1(inputStream, 9).debug *> part2(inputStream, 9).debug
