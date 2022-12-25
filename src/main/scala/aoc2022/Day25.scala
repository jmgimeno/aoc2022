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

  extension (base5: String)
    def add(other5: String): String =
      base5
        .zip(other5)
        .foldRight(("", 0L)) { case ((b5, d5), (right, carry)) =>
          val sum = b5.toDigit5 + d5.toDigit5 + carry
          ((sum % 5L).toString + right, sum / 5L)
        }
        ._1

    def minus(other5: String): String =
      base5
        .zip(other5)
        .foldRight(("", 0L)) { case ((b5, d5), (right, carry)) =>
          val sum = b5.toDigit5 - d5.toDigit5 + carry
          sum match
            case 4L  => ("-" ++ right, 1L)
            case 3L  => ("=" ++ right, 1L)
            case -2L => ("=" ++ right, 0L)
            case -1L => ("-" ++ right, 0L)
            case n   => (s"$n$right", 0)
        }
        ._1

  extension (base10: Long)
    def toDigitSnafu = base10 match
      case -2L => '='
      case -1L => '-'
      case 0L  => '0'
      case 1L  => '1'
      case 2L  => '2'

    def to5: String =
      List
        .unfold(base10) { n =>
          if (n == 0) then None else Some(n % 5L, n / 5L)
        }
        .reverse
        .mkString("0", "", "")

    def toSnafu: String =
      val base5 = base10.to5
      val ones = "0" + "1" * (base5.length - 1)
      val step1 = base5.add(ones)
      val step2 = step1.minus(ones)
      if (step2.length > 1 && step2(0) == '0') then step2.substring(1) else step2

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
