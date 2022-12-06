import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day6.*

object Day6Suite extends ZIOSpecDefault:

  val example1 =
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

  val example2 =
    "bvwbjplbgvbhsrlpgdmjqwftvncz"

  val example3 =
    "nppdvjthqldpwncqszvftbrmjlhg"

  val example4 =
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

  val example5 =
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

  extension (s: String) def mks = ZStream.fromIterable(s).map(_.toString)

  lazy val spec =
    suite("day6")(
      suite("part1")(
        test("examples") {
          assertZIO(part1(example1.mks))(equalTo(7))
          assertZIO(part1(example2.mks))(equalTo(5))
          assertZIO(part1(example3.mks))(equalTo(6))
          assertZIO(part1(example4.mks))(equalTo(10))
          assertZIO(part1(example5.mks))(equalTo(11))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(1929))
        }
      ),
      suite("part2")(
        test("examples") {
          assertZIO(part2(example1.mks))(equalTo(19))
          assertZIO(part2(example2.mks))(equalTo(23))
          assertZIO(part2(example3.mks))(equalTo(23))
          assertZIO(part2(example4.mks))(equalTo(29))
          assertZIO(part2(example5.mks))(equalTo(26))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(3298))
        }
      )
    )
