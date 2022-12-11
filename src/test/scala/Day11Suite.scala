import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day11.*

object Day11Suite extends ZIOSpecDefault:

  val example =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day11")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(10605))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(56595))
        }
      ),
      suite("part2")(
        test("example.txt") {
          assertZIO(part2(exampleStream))(equalTo(BigInt("2713310158")))
        } @@ ignore,
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(-1))
        } @@ ignore
      )
    )
