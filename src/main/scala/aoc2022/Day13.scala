package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps
import math.Ordered.orderingToOrdered

object Day13 extends ZIOAppDefault:

  enum IntTree:
    case Leaf(n: Int)
    case Branch(children: List[IntTree])

  object IntTree:
    given Ordering[IntTree] with
      def compare(left: IntTree, right: IntTree) = (left, right) match
        case (Leaf(lv), Leaf(rv))           => lv - rv
        case (l @ Leaf(lv), r @ Branch(rc)) => compare(Branch(List(l)), r)
        case (l @ Branch(lc), r @ Leaf(rv)) => compare(l, Branch(List(r)))
        case (Branch(lc), Branch(rc)) =>
          lc.zip(rc).map(compare).find(_ != 0).getOrElse(lc.length - rc.length)

  extension (c: Char)
    def isBranchStart = c == '['
    def isBranchEnd = c == ']'
    def isComma = c == ','
    def isLeafStart = c.isDigit
    def isLeafEnd = c.isBranchEnd || c.isComma

  object Parser:

    def parseBlock(block: Chunk[String]) =
      block.map(parseIntTree)

    def parseIntTree(input: String): IntTree =
      if input(0).isBranchStart then parseBranch(input)._1
      else if input(0).isLeafStart then parseLeaf(input)._1
      else assert(false, "Shouldn't happen")

    private def parseBranch(input: String): (IntTree, String) =
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

    private def parseLeaf(input: String) =
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
      .filter((chunk, _) => chunk(0) < chunk(1))
      .map((_, index) => index + 1)
      .runSum

  def multiplyMarkerPositions(trees: List[IntTree]): Int =
    List(2, 6)
      .map(num => Parser.parseIntTree(s"[[$num]]"))
      .map(marker => trees.indexOf(marker) + 1)
      .product

  def part2(is: UStream[String]): Task[Long] =
    (is ++ ZStream("[[2]]", "[[6]]"))
      .filterNot(_.isEmpty)
      .map(Parser.parseIntTree)
      .runCollect
      .map(_.toList.sorted)
      .map(multiplyMarkerPositions)

  lazy val run =
    part1(inputStream).debug("PART1") *> part2(inputStream).debug("PART2")
