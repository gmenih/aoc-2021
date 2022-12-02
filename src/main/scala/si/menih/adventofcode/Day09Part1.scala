package si.menih.adventofcode
package Day9Part1

import si.menih.adventofcode.lib.FileHelper

object LowPointsFinder:
  def find(lines: Seq[Seq[Int]]) =
    def isLowPoint(pointX: Int, pointY: Int): Boolean =
      val adjacentPoints = for {
        x <- pointX - 1 to pointX + 1
        y <- pointY - 1 to pointY + 1
        if (x == pointX || y == pointY) &&
        !(x == pointX && y == pointY) &&
        ((x >= 0 && x < lines.size) &&
        (y >= 0 && y < lines(x).size))
      } yield lines(x)(y)

      val v = lines(pointX)(pointY)

      adjacentPoints.forall(_ > v)

    val lowPoints = for {
      x <- 0 until lines.size
      y <- 0 until lines(x).size
      if isLowPoint(x, y)
    } yield lines(x)(y)

    lowPoints.map(_ + 1).sum

@main def Day09(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day09.txt")
  val lines = inputLines.map(_.split("").map(_.toInt).toSeq)

  val solution = LowPointsFinder.find(lines)

  println(s"Solution: $solution")
