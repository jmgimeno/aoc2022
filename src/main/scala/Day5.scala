import zio.*
import zio.stream.*

object Day5 extends ZIOAppDefault:

  type Stack = String
  type Crate = Char
  type Level = Seq[(Crate, Int)]

  final case class Stacks private (stacks: IndexedSeq[Stack]):

    private def moving(
        stacks: IndexedSeq[Stack]
    )(size: Int, from: Int, to: Int): IndexedSeq[Stack] =
      val crate = stacks(from - 1).takeRight(size)
      val newFrom = stacks(from - 1).dropRight(size)
      val newTo = stacks(to - 1) ++ crate
      stacks.updated(from - 1, newFrom).updated(to - 1, newTo)

    def movingSingle(moves: Chunk[Move]): Stacks =
      Stacks {
        moves.foldLeft(stacks) { case (stacks, Move(n, from, to)) =>
          (1 to n).foldLeft(stacks) { (stacks, _) =>
            moving(stacks)(1, from, to)
          }
        }
      }

    def movingGroup(moves: Chunk[Move]): Stacks =
      Stacks {
        moves.foldLeft(stacks) { case (stacks, Move(n, from, to)) =>
          moving(stacks)(n, from, to)
        }
      }

    def tops: String =
      stacks.map(_.last).mkString

  object Stacks:
    def apply(numStacks: Int, levels: Chunk[Level]): Stacks =
      val stacks = IndexedSeq.fill(numStacks)("")
      Stacks {
        levels.foldLeft(stacks) { (stacks, level) =>
          level.foldLeft(stacks) { case (stacks, (crate, stack)) =>
            val old = stacks(stack - 1)
            stacks.updated(stack - 1, old.prepended(crate))
          }
        }
      }

  final case class Move(n: Int, from: Int, to: Int)

  def parseNumStacks(line: String): Int =
    raw"(\d)+".r.findAllMatchIn(line).toSeq.last.group(1).toInt

  def parseStackLevel(numStacks: Int)(line: String): Level =
    val positions = (0 until numStacks).map(_ * 4 + 1)
    val crates = positions.map(line.charAt)
    crates.zipWithIndex.filter(_._1 != ' ').map((c, i) => (c, i + 1))

  def parseStacks(lines: Chunk[String], numStacks: Int): Stacks =
    val levels = lines.map(parseStackLevel(numStacks))
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
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  def part[R, E](
      is: ZStream[R, E, String],
      movement: (Stacks, Chunk[Move]) => Stacks
  ): ZIO[R, E, String] =
    for
      parts <- is.split(_.isEmpty()).runCollect
      numStacks = parseNumStacks(parts(0).last)
      stacks = parseStacks(parts(0).dropRight(1), numStacks)
      moves = parseMoves(parts(1))
    yield movement(stacks, moves).tops

  def part1[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, String] =
    part(is, _ movingSingle _)

  def part2[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, String] =
    part(is, _ movingGroup _)

  lazy val run = part1(inputStream).debug *> part2(inputStream).debug
