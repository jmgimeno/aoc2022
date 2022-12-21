package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day21.*

object Day21Suite extends ZIOSpecDefault:

  lazy val example =
    """root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32""".stripMargin

  lazy val exampleStream = ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day21")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(152L))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(158731561459602L))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(0))
        } @@ ignore,
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(0))
        } @@ ignore
      )
    )
