package si.menih.adventofcode
package Day7Part1

import si.menih.adventofcode.lib.FileHelper

type Positions = Seq[Int];

object Crabs:
  def fuelConsumption (positions: Positions, target: Int): Int =
    positions.map(v => (v - target).abs).sum

  def findSolution (list: Positions): Int =
    val target = search(list, list.length / 2)
    fuelConsumption(list, target)

  private def search (list: Positions, target: Int): Int =
    val diff = 1
    val f = fuelConsumption(list, target)

    if (fuelConsumption(list, target - diff) < f)
      search(list, target - diff)
    else if (fuelConsumption(list, target + diff) < f)
      search(list, target + diff)
    else
      target

@main def Day07(args: String*): Unit =
  val inputLine: String = FileHelper.readInputLines("day07.txt").head

  val crabPositions: Positions = inputLine.split(",").map(_.toInt).sorted.toSeq;
  val solution = Crabs.findSolution(crabPositions)

  println(s"Solution: $solution")
