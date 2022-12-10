import zio.*
import zio.stream.*

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

  case class Position(x: Int, y: Int):
    def up = Position(x, y + 1)
    def right = Position(x + 1, y)
    def down = Position(x, y - 1)
    def left = Position(x - 1, y)

    def distance(other: Position): Int =
      math.abs(x - other.x) + math.abs(y - other.y)

    def touching(other: Position): Boolean =
      if this == other then true
      else if x == other.x then distance(other) == 1
      else if y == other.y then distance(other) == 1
      else distance(other) == 2

    def twoStepsSameAxis(other: Position): Boolean =
      (x == other.x || y == other.y) && distance(other) == 2

    def sameMovement(current: Position, next: Position): Position =
      if x == next.x
      then Position(x, y + next.y - current.y)
      else Position(x + next.x - current.x, y)

    def moveDiagonal(towards: Position): Position =
      val dx = if x < towards.x then +1 else -1
      val dy = if y < towards.y then +1 else -1
      Position(x + dx, y + dy)

  case class Rope private (
      knots: List[Position],
      visited: Set[Position]
  ):

    private def moveTail(
        oldTail: Position,
        oldHead: Position,
        newHead: Position
    ): Position =
      if newHead.twoStepsSameAxis(oldTail) then
        oldTail.sameMovement(oldHead, newHead)
      else if !newHead.touching(oldTail) then oldTail.moveDiagonal(newHead)
      else oldTail

    private def move(newHead: Position): Rope =
      val newKnots = knots.tail
        .foldLeft(List((knots.head, newHead))) { (initKnots, oldTail) =>
          val (oldHead, newHead) = initKnots.head
          val newTail = moveTail(oldTail, oldHead, newHead)
          (oldTail, newTail) :: initKnots
        }
        .map(_._2)
        .reverse
      Rope(newKnots, visited + newKnots.last)

    def up: Rope =
      move(knots.head.up)

    def right: Rope =
      move(knots.head.right)

    def down: Rope =
      move(knots.head.down)

    def left: Rope =
      move(knots.head.left)

  object Rope:
    def make(n: Int = 1): Rope = Rope(
      knots = List.fill(n)(Position(0, 0)),
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

  def part[R, E](numKnots: Int)(
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    for rope <- is
        .map(Motion.parse)
        .runFold(Rope.make(numKnots))(Rope.doMotion)
    yield rope.visited.size

  val part1 = part(2)
  val part2 = part(10)

  val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
