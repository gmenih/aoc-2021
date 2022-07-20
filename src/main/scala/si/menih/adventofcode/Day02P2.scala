package si.menih
package adventofcode
package Day2Part2

import si.menih.adventofcode.lib.FileHelper

enum Command:
  case Forward(v: Int)
  case Down(v: Int)
  case Up(v: Int)

object Command:
  def from(s: String): Command =
    s match
      case s"forward $v" if v.toIntOption.isDefined => Forward(v.toInt)
      case s"down $v" if v.toIntOption.isDefined => Down(v.toInt)
      case s"up $v" if v.toIntOption.isDefined => Up(v.toInt)
      case _ => throw new Exception(s"Invalid command: $s")

case class Point(
    h: Int,
    d: Int,
    a: Int,
  ):
  def move(c: Command): Point =
    c match
      case Command.Forward(v) => Point(h + v, d + (a * v), a)
      case Command.Down(v) => Point(h, d, a + v)
      case Command.Up(v) => Point(h, d, a - v)

  def solution: Int = h * d

@main def Day02Part2(args: String*): Unit =
  val inputs: List[Command] = FileHelper.readInputLines("day02.txt").map(Command.from)
  val pos = Point(0, 0, 0)
  val last = inputs.foldLeft(pos)((p, c) => p.move(c))

  println(s"The solution: ${last.solution}")
