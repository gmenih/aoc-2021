package si.menih.adventofcode
package Day10Part2

import si.menih.adventofcode.lib.FileHelper
import scala.collection.mutable.Stack
import scala.util.control.Exception.*

class UnmatchedBracketException(val s: String, val c: Char) extends Exception(s"$c")


object BracketMaster9000:
  def score (c: Char): Int =
    c match
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
      case _ => 0

  val matches = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  def validate(s: String): Long =
    val stack = Stack[Char]()

    for c <- s do
      if matches.contains(c) then
        stack.push(c)
      else if stack.nonEmpty && matches(stack.top) == c then
        stack.pop()
      else if matches.valuesIterator.contains(c) then
        throw new UnmatchedBracketException(s, c)


    stack.foldLeft[Long](0)((acc, c) => acc * 5 + score(matches(c)))


@main def Day09(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day10.txt")

  val result = for {
    line <- inputLines
    result <- allCatch opt BracketMaster9000.validate(line)
  } yield result

  val sorted = result.sorted
  println(s"Middle item: ${sorted.apply(sorted.length / 2)}")
