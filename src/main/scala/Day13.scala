import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day13 extends ZIOAppDefault:

  enum IntTree:
    case Leaf(n: Int)
    case Branch(children: List[IntTree])

    def compareTo(other: IntTree): Int = (this, other) match
      case (Leaf(lv), Leaf(rv))           => lv - rv
      case (l @ Leaf(lv), r @ Branch(rc)) => Branch(List(l)).compareTo(r)
      case (l @ Branch(lc), r @ Leaf(rv)) => l.compareTo(Branch(List(r)))
      case (Branch(lc), Branch(rc)) =>
        val comparisons = lc.zip(rc).map(_ compareTo _)
        if comparisons.forall(_ == 0) then lc.length - rc.length
        else comparisons.find(_ != 0).get

  object IntTree:
    given Ordering[IntTree] with
      def compare(left: IntTree, right: IntTree) =
        left.compareTo(right)

  extension (c: Char)
    def isEnd = c == '$'
    def isBranchStart = c == '['
    def isBranchEnd = c == ']'
    def isComma = c == ','
    def isLeafStart = c.isDigit
    def isLeafEnd = c.isBranchEnd || c.isComma

  object Parser:

    def parseBlock(block: Chunk[String]) =
      block.map(s => parseIntTree(s + "$")._1)

    def parseIntTree(input: String): (IntTree, String) =
      if input(0).isBranchStart then parseBranch(input)
      else if input(0).isLeafStart then parseLeaf(input)
      else assert(false, "Shouldn't happen")

    def parseBranch(input: String): (IntTree, String) =
      assert(input(0).isBranchStart)
      val children = mutable.ListBuffer.empty[IntTree]
      var rest = input.drop(1)
      while !rest(0).isBranchEnd do
        if rest(0).isComma then rest = rest.drop(1)
        else if rest(0).isLeafStart then
          val (leaf, newRest) = parseLeaf(rest)
          children += leaf
          rest = newRest
        else if rest(0).isBranchStart then
          val (branch, newRest) = parseBranch(rest)
          children += branch
          rest = newRest
      (IntTree.Branch(children.toList), rest.drop(1))

    def parseLeaf(input: String) =
      assert(input(0).isLeafStart)
      val (number, rest) = input.span(_.isDigit)
      (IntTree.Leaf(number.toInt), rest)

  def compare(trees: Chunk[IntTree], index: Long) =
    trees(0).compareTo(trees(1))

  val inputStream =
    ZStream
      .fromFileName("data/input13.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .orDie

  def part1(is: UStream[String]): Task[Long] =
    is.split(_.isEmpty)
      .map(Parser.parseBlock)
      .zipWithIndex
      .filter((chunk, _) => chunk(0).compareTo(chunk(1)) < 0)
      .map((_, index) => index + 1)
      .runSum

  def part2(trees: List[IntTree]): Int =
    List(2, 6)
      .map(num => Parser.parseIntTree(s"[[$num]]")._1)
      .map(marker => trees.indexOf(marker) + 1)
      .product

  def part2(is: UStream[String]): Task[Long] =
    (is ++ ZStream("[[2]]", "[[6]]"))
      .filterNot(_.isEmpty)
      .map(Parser.parseIntTree(_)._1)
      .runCollect
      .map(_.toList.sorted)
      .map(part2)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
