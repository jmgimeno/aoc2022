import zio.*
import zio.test.*
import zio.test.Assertion.*

import Day3.*

object Day3Suite extends ZIOSpecDefault:

  def spec =
    suite("part1")(
      test("priorities") {
        assertTrue(priority("vJrwpWtwJgWrhcsFMMfFFhFp") == 16)
        assertTrue(priority("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL") == 38)
        assertTrue(priority("PmmdzqPrVvPwwTWBwg") == 42)
        assertTrue(priority("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn") == 22)
        assertTrue(priority("ttgJtRGJQctTZtZT") == 20)
        assertTrue(priority("CrZsJsPPZsGzwwsLwLmpwMDw") == 19)
      },
      test("part1") {
        assertZIO(part1)(equalTo(7821))
      },
      test("group") {
        assertTrue(
          priority(
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg"
          ) == 18
        )
        assertTrue(
          priority(
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw"
          ) == 52
        )
      },
      test("part2") {
        assertZIO(part2)(equalTo(2752))
      }
    )
