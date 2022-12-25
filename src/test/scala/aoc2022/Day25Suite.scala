package aoc2022

import zio.*
import zio.stream.*
import zio.test.*

import zio.test.Assertion.*
import zio.test.TestAspect.*

import Day25.*

object Day25Suite extends ZIOSpecDefault:

  val example =
    """1=-0-2
      |12111
      |2=0=
      |21
      |2=01
      |111
      |20012
      |112
      |1=-1=
      |1-12
      |12
      |1=
      |122""".stripMargin

  val exampleStream = ZStream.fromIterable(example.split("\n"))

  val spec =
    suite("day25")(
      test("roundtrip")(
        check(Gen.long(1, 1_000_000_000)) { n =>
          assert(n.toSnafu.to10)(equalTo(n))
        }
      ),
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo("2=-1=0"))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo("20=022=21--=2--12=-2"))
        }
      )
    )
