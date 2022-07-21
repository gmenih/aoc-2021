package si.menih.adventofcode
package Day6

import si.menih.adventofcode.lib.FileHelper

object Fish:
  var fishCount: Map[Int, Long] = Map()

  def parse (list: Seq[Int]): Unit =
    fishCount = list.groupBy(identity).map((f, s) => (f, s.size.toLong))

  def passDay(): Unit =
    val newFish: Long = fishCount.getOrElse(0, 0l)

    for (i <- 0 to 7)
      fishCount += (i -> fishCount.getOrElse(i + 1, 0))

    fishCount += (6 -> (fishCount.getOrElse(6, 0l) + newFish))
    fishCount += (8 -> newFish)


  def solution(): Long =
    fishCount.values.sum

@main def Day06(args: String*): Unit =
  val inputLine: String = FileHelper.readInputLines("day06.txt").head
  val firstGen = inputLine.split(",").map(_.toInt).toSeq

  Fish.parse(firstGen)

  for (_ <- 1 to 80)
    Fish.passDay()

  println(s"Solution Part 1: ${Fish.solution()}")

  Fish.parse(firstGen)
  for (_ <- 1 to 256)
    Fish.passDay()

  println(s"Solution Part 2: ${Fish.solution()}")
