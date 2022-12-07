import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import Day7.*

object Day7Suite extends ZIOSpecDefault:

  lazy val example =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin

  val exampleStream =
    ZStream.fromIterable(example.split("\n"))

  lazy val spec =
    suite("day6")(
      suite("part1")(
        test("example") {
          assertZIO(part1(exampleStream))(equalTo(95437))
        },
        test("input.txt") {
          assertZIO(part1(inputStream))(equalTo(1491614))
        }
      ),
      suite("part2")(
        test("example") {
          assertZIO(part2(exampleStream))(equalTo(24933642))
        },
        test("input.txt") {
          assertZIO(part2(inputStream))(equalTo(6400111))
        }
      )
    )
