import zio.*
import zio.stream.*

import scala.collection.mutable
import java.io.File
import scala.annotation.newMain

object Day9 extends ZIOAppDefault:

  enum Motion:
    case Up(steps: Int)
    case Right(steps: Int)
    case Down(steps: Int)
    case Left(steps: Int)

  object Motion:
    def parse(line: String): Motion = line match
      case s"U $steps" => Up(steps.toInt)
      case s"R $steps" => Right(steps.toInt)
      case s"D $steps" => Down(steps.toInt)
      case s"L $steps" => Left(steps.toInt)

  enum Diagonal(val dx: Int, val dy: Int):
    case RightDown extends Diagonal(1, 1)
    case RightUp extends Diagonal(1, -1)
    case LeftDown extends Diagonal(-1, 1)
    case LeftUp extends Diagonal(-1, -1)

  case class Position(x: Int, y: Int):
    def up = Position(x, y - 1)
    def right = Position(x + 1, y)
    def down = Position(x, y + 1)
    def left = Position(x - 1, y)

    def touching(other: Position) =
      if this == other then true
      else if x == other.x then math.abs(y - other.y) == 1
      else if y == other.y then math.abs(x - other.x) == 1
      else math.abs(x - other.x) + math.abs(y - other.y) == 2

    def twoStepsURDL(other: Position): Boolean =
      val steps = math.abs(x - other.x) + math.abs(y - other.y)
      val ud = x == other.x
      val rl = y == other.y
      steps == 2 && (ud || rl)

    def diagonal(diagonal: Diagonal): Position =
      Position(x + diagonal.dx, y + diagonal.dy)

  case class Rope private (
      head: Position,
      tail: Position,
      visited: Set[Position]
  ):
    val sameRow = head.y == tail.y
    val sameCol = head.x == tail.x
    val covered = head == tail
    val diagonal = !sameCol || !sameRow

    def up: Rope =
      val newHead = head.up
      val newTail =
        if newHead.twoStepsURDL(tail) then tail.up
        else if !newHead.touching(tail) then
          if tail.x < head.x then tail.diagonal(Diagonal.RightUp)
          else tail.diagonal(Diagonal.LeftUp)
        else tail
      Rope(newHead, newTail, visited + newTail)

    def right: Rope =
      val newHead = head.right
      val newTail =
        if newHead.twoStepsURDL(tail) then tail.right
        else if !newHead.touching(tail) then
          if tail.y < head.y then tail.diagonal(Diagonal.RightDown)
          else tail.diagonal(Diagonal.RightUp)
        else tail
      Rope(newHead, newTail, visited + newTail)

    def down: Rope =
      val newHead = head.down
      val newTail =
        if newHead.twoStepsURDL(tail) then tail.down
        else if !newHead.touching(tail) then
          if tail.x < head.x then tail.diagonal(Diagonal.RightDown)
          else tail.diagonal(Diagonal.LeftDown)
        else tail
      Rope(newHead, newTail, visited + newTail)

    def left: Rope =
      val newHead = head.left
      val newTail =
        if newHead.twoStepsURDL(tail) then tail.left
        else if !newHead.touching(tail) then
          if tail.y < head.y then tail.diagonal(Diagonal.LeftDown)
          else tail.diagonal(Diagonal.LeftUp)
        else tail
      Rope(newHead, newTail, visited + newTail)

  object Rope:
    def make: Rope = Rope(
      head = Position(0, 0),
      tail = Position(0, 0),
      visited = Set(Position(0, 0))
    )

    private def repeat[A](n: Int)(init: A)(f: A => A): A =
      (1 to n).foldLeft(init) { (a, _) => f(a) }

    def doMotion(rope: Rope, move: Motion): Rope = move match
      case Motion.Up(steps)    => repeat(steps)(rope)(_.up)
      case Motion.Right(steps) => repeat(steps)(rope)(_.right)
      case Motion.Down(steps)  => repeat(steps)(rope)(_.down)
      case Motion.Left(steps)  => repeat(steps)(rope)(_.left)

  val inputStream =
    ZStream
      .fromFileName("data/input9.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  def part1[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    for rope <- is
        .map(Motion.parse)
        .runFold(Rope.make)(Rope.doMotion)
    yield rope.visited.size

  // def part2[R, E](
  //     is: ZStream[R, E, String]
  // ): ZIO[R, E, Int] =
  //   ???

  val run = part1(inputStream).debug("PART1")
