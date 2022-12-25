package aoc2022

import zio.*
import zio.stream.*

import scala.util.chaining.scalaUtilChainingOps

object Day25 extends ZIOAppDefault:

  extension (snafu: Char)
    def toDigit10 = (snafu match
      case '=' => -2
      case '-' => -1
      case '0' => 0
      case '1' => 1
      case '2' => 2
    ).toLong

  extension (base5: Char) def toDigit5 = base5.toInt - '0'.toInt

  extension (snafu: String)
    def to10: Long =
      snafu.foldLeft(0L) { (base10, snafuDigit) =>
        5 * base10 + snafuDigit.toDigit10
      }

  extension (base10: Long)
    def toSnafu: String =
      if base10 == 0 then ""
      else
        val r = base10 % 5
        if (0 <= r && r <= 2) then (base10 / 5).toSnafu ++ r.toString
        else (base10 / 5 + 1).toSnafu ++ (if r == 3 then "=" else "-")

  lazy val inputStream =
    ZStream
      .fromFileName("data/input25.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[String] =
    is.map(_.to10)
      .runSum
      .map(_.toSnafu)

  lazy val run =
    part1(inputStream).debug("PART1")
