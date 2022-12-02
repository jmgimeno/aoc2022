import zio.*
import zio.stream.*

object Day2 extends ZIOAppDefault:

  val inputStream =
    ZStream
      .fromFileName("data/input2.txt")
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)

  enum RPS:
    case Rock, Paper, Scissors

  import RPS.*

  extension (rps: RPS)
    def winsTo: RPS =
      val wt = Map(Rock -> Scissors, Paper -> Rock, Scissors -> Paper)
      wt(rps)
    def lossesTo: RPS =
      val lt = Map(Rock -> Paper, Paper -> Scissors, Scissors -> Rock)
      lt(rps)
    def points: Int =
      val score = Map(Rock -> 1, Paper -> 2, Scissors -> 3)
      score(rps)

  enum Outcome:
    case Loss, Draw, Win

  import Outcome.*

  extension (oc: Outcome)
    def points: Int =
      val score = Map(Loss -> 0, Draw -> 3, Win -> 6)
      score(oc)

  val parseYou = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)

  val parseMe = Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors)

  case class Part1Round(you: RPS, me: RPS):
    def outcome: Outcome =
      if me == you then Draw
      else if me.winsTo == you then Win
      else Loss
    def score: Int = me.points + outcome.points

  object Part1Round:
    def parse(line: String): Part1Round =
      val parts = line.split(" ")
      Part1Round(parseYou(parts(0).trim), parseMe(parts(1).trim))

  def part1[R, E](inputStream: ZStream[R, E, String]): ZIO[R, E, Int] =
    inputStream
      .map(Part1Round.parse)
      .map(_.score)
      .runFold(0)(_ + _)

  val parseOutcome =
    Map("X" -> Loss, "Y" -> Draw, "Z" -> Win)

  case class Part2Round(you: RPS, out: Outcome):
    def choose: RPS =
      if out == Draw then you
      else if out == Win then you.lossesTo
      else you.winsTo
    def score: Int = choose.points + out.points

  object Part2Round:
    def parse(line: String): Part2Round =
      val parts = line.split(" ")
      Part2Round(parseYou(parts(0).trim), parseOutcome(parts(1).trim))

  def part2[R, E](inputStream: ZStream[R, E, String]): ZIO[R, E, Int] =
    inputStream
      .map(Part2Round.parse)
      .map(_.score)
      .runFold(0)(_ + _)

  val run = part1(inputStream).debug *> part2(inputStream).debug
