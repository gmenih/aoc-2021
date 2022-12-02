package si.menih.adventofcode
package Day10Part1

import si.menih.adventofcode.lib.FileHelper
import scala.collection.mutable.Stack

class UnmatchedBracketException(val s: String, val c: Char) extends Exception(s"$c"):
  def score (): Int =
    c match
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137

object BracketMaster9000:
  val matches = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  def validate(s: String): Int =
    val stack = Stack[Char]()

    for c <- s do
      if matches.contains(c) then
        stack.push(c)
      else if stack.nonEmpty && matches(stack.top) == c then
        stack.pop()
      else if matches.valuesIterator.contains(c) then
        throw new UnmatchedBracketException(s, c)

    0

@main def Day09(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day10.txt")

  val result = for (line <- inputLines) yield
    try
      BracketMaster9000.validate(line)
    catch
      case e: UnmatchedBracketException => e.score()

  println(result.sum)

