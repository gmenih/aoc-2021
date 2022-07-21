package si.menih.adventofcode
package Day4

import si.menih.adventofcode.lib.FileHelper
import scala.collection.StringOps

class Board(_board: Seq[String]):
  val board: Seq[Int] = _board.map(_.trim.replaceAll(" {2,}", " ").split(" ").map(_.toInt)).flatten
  var hits: Seq[Boolean] = Array.ofDim[Boolean](board.length).toSeq
  var winningNumber: Int = -1

  def bingo: Boolean = winningNumber != -1

  def play(n: Int): Boolean =
    val i = board.indexOf(n)
    if (i == -1) false
    else
      hits = hits.updated(i, true)
      val isBingo = checkBingo(i)
      if (isBingo)
        winningNumber = n

      isBingo

  def score: Int =
    val unmarkedSum = hits.zipWithIndex.filter((v, i) => !v).map((v, i) => board(i)).sum

    unmarkedSum * winningNumber

  private def checkBingo(i: Int): Boolean =
    val rowStart = (i / 5) * 5;
    val colStart = i % 5
    val colBingo = (colStart until board.length by 5).foldLeft(true)((v, i) => v & hits(i));
    val rowBingo = (rowStart until (rowStart + 5)).foldLeft(true)((v, i) => v & hits(i));

    rowBingo || colBingo

@main def Day04(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day04.txt")

  val commands = inputLines(0).split(",").map(_.toInt)
  // Skip commands + empty line, group into 6 lines dropping last empty line
  val boards = inputLines.drop(2).grouped(6).map(_.filter(_ != "")).map(Board(_)).toSeq

  val winners = for {
    command <- commands;
    board <- boards;
    if !board.bingo && board.play(command)
  } yield board

  // Part 1
  println(winners.head.score)
  // Part 2
  println(winners.last.score)
