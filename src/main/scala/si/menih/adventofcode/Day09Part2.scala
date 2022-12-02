package si.menih.adventofcode
package Day9Part2

import si.menih.adventofcode.lib.FileHelper

object BasinFinder:
  def findBasin (x: Int, y: Int): Seq[(Int, Int)] =
    ???


  def find (lines: Seq[Seq[Int]]): Seq[Seq[(Int, Int)]] =
    val checked: Set[(Int, Int)] = Set.empty
    for {
      y <- 0 until lines.size
      x <- 0 until lines(y).size
      if lines(y)(x) != 9
    } yield findBasin(x, y)



@main def Day09(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day09.txt")
  val lines = inputLines.map(_.split("").map(_.toInt).toSeq)

