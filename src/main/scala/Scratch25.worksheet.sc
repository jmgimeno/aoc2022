extension (snafu: Char)
  def toDigit10 = snafu match
    case '=' => -2
    case '-' => -1
    case '0' => 0
    case '1' => 1
    case '2' => 2

extension (base5: Char) def toDigit5 = base5.toInt - '0'.toInt

extension (snafu: String)
  def to10: Long =
    snafu.foldLeft(0) { (base10, snafuDigit) =>
      5 * base10 + snafuDigit.toDigit10
    }

extension (base5: String)
  def add(other5: String): String =
    base5
      .zip(other5)
      .foldRight(("", 0)) { case ((b5, d5), (right, carry)) =>
        val sum = b5.toDigit5 + d5.toDigit5 + carry
        ((sum % 5).toString + right, sum / 5)
      }
      ._1

  def minus(other5: String): String =
    base5
      .zip(other5)
      .foldRight(("", 0)) { case ((b5, d5), (right, carry)) =>
        val sum = b5.toDigit5 - d5.toDigit5 + carry
        sum match
          case 4  => ("-" ++ right, 1)
          case 3  => ("=" ++ right, 1)
          case -2 => ("=" ++ right, 0)
          case -1 => ("-" ++ right, 0)
          case n  => (s"$n$right", 0)
      }
      ._1

extension (base10: Long)
  def toDigitSnafu = base10 match
    case -2 => '='
    case -1 => '-'
    case 0  => '0'
    case 1  => '1'
    case 2  => '2'

  def to5: String =
    List
      .unfold(base10) { n =>
        if (n == 0) then None else Some(n % 5, n / 5)
      }
      .reverse
      .mkString("0", "", "")

  def toSnafu: String =
    val base5 = base10.to5
    val ones = "0" + "1" * (base5.length - 1)
    val step1 = base5.add(ones)
    val step2 = step1.minus(ones)
    println(s"base5: $base5")
    println(s"ones : $ones")
    println(s"step1: $step1")
    println(s"step2: $step2")
    if (step2.length > 1 && step2(0) == '0') then step2.substring(1) else step2

0.to5
0.toSnafu
1.to5
1.toSnafu
2.to5
2.toSnafu
3.to5
3.toSnafu
4.to5
4.toSnafu
(0 to 20).map(n => (n -> n.toSnafu))
