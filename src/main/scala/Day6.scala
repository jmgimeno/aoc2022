import zio.*
import zio.stream.*

object Day6 extends ZIOAppDefault:

  val inputStream =
    ZStream
      .fromFileName("data/input6.txt")
      .via(ZPipeline.utf8Decode)
      .flatMap(ZStream.fromIterable)
      .map(_.toString)

  def part[R, E](size: Int)(
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    is
      .sliding(size, 1)
      .zip(ZStream.iterate(size)(_ + 1))
      .find(_._1.distinct.size == size)
      .map(_._2)
      .runCollect
      .map(_.head)

  val part1 = part(4)
  val part2 = part(14)

  lazy val run = part1(inputStream).debug *> part2(inputStream).debug
