package si.menih.adventofcode
package Day5Part2

import si.menih.adventofcode.lib.FileHelper
import scala.collection.StringOps

case class Point(val x: Int, val y: Int)

case class Line(
    val x1: Int,
    y1: Int,
    x2: Int,
    y2: Int,
  ):
  def toPoints: Seq[Point] =
    val yRange = y1 to y2 by (if y1 > y2 then -1 else 1)
    val xRange = x1 to x2 by (if x1 > x2 then -1 else 1)
    if (x1 == x2)
      yRange.map(y => Point(x1, y)).toSeq
    else if (y1 == y2)
      xRange.map(x => Point(x, y1)).toSeq
    else
      for ((x, y) <- xRange.zip(yRange)) yield Point(x, y)

@main def Day05Part2(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day05.txt")

  val lines = for (line <- inputLines) yield line match
    case s"$x1,$y1 -> $x2,$y2" => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)

  val intersectionPoints = (for (l1 <- lines) yield l1.toPoints).flatten
  val multiIntersectionCount =
    intersectionPoints.groupBy(identity).map((a, b) => b.size).filter(_ > 1).size

  println(s"Solution: $multiIntersectionCount")
