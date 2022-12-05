import zio.*
import zio.stream.*

object Day5 extends ZIOAppDefault:

  type Stack = String
  type Level = Seq[(Char, Int)]

  final case class Stacks private (stacks: IndexedSeq[Stack]):

    def execute(moves: Chunk[Move]): Stacks =
      Stacks {
        moves.foldLeft(stacks) { case (stacks, Move(n, from, to)) =>
          (1 to n).foldLeft(stacks) { (stacks, _) =>
            val top = stacks(from - 1).last
            val newFrom = stacks(from - 1).dropRight(1)
            val newTo = stacks(to - 1).appended(top)
            stacks.updated(from - 1, newFrom).updated(to - 1, newTo)
          }
        }
      }

    def execute2(moves: Chunk[Move]): Stacks =
      Stacks {
        moves.foldLeft(stacks) { case (stacks, Move(n, from, to)) =>
          val top = stacks(from - 1).takeRight(n)
          val newFrom = stacks(from - 1).dropRight(n)
          val newTo = stacks(to - 1) ++ top
          stacks.updated(from - 1, newFrom).updated(to - 1, newTo)
        }
      }

    def tops: String =
      stacks.map(_.last).mkString

  end Stacks

  object Stacks:
    def apply(numStacks: Int, levels: Chunk[Level]): Stacks =
      val stacks = IndexedSeq.fill(numStacks)("")
      Stacks {
        levels.foldLeft(stacks) { (stacks, level) =>
          level.foldLeft(stacks) { case (stacks, (c, s)) =>
            val old = stacks(s - 1)
            stacks.updated(s - 1, old.prepended(c))
          }
        }
      }

  final case class Move(n: Int, from: Int, to: Int)

  def parseStackLevel(numStacks: Int)(line: String): Level =
    val positions = (0 until numStacks).map(i => i * 4 + 1)
    val crates = positions.map(line.charAt(_))
    crates.zipWithIndex.filter(_._1 != ' ').map((c, i) => (c, i + 1))

  def parseStacks(lines: Chunk[String], numStacks: Int): Stacks =
    val levels = lines.dropRight(1).map(parseStackLevel(numStacks))
    Stacks(numStacks, levels)

  def parseMoves(lines: Chunk[String]): Chunk[Move] =
    lines.map { line =>
      val movePattern = """^move (\d+) from (\d+) to (\d+)$""".r
      val m = movePattern.findAllIn(line)
      Move(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt)
    }

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
    yield stacks.execute(moves).tops

  def part2[R, E](
      is: ZStream[R, E, String],
      numStacks: Int = 9
  ): ZIO[R, E, String] =
    for
      parts <- is.split(_.isEmpty()).runCollect
      stacks = parseStacks(parts(0), numStacks)
      moves = parseMoves(parts(1))
    yield stacks.execute2(moves).tops

  lazy val run = part1(inputStream, 9).debug *> part2(inputStream, 9).debug
