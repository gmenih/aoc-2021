package si.menih
package adventofcode
package Day1Part1

import si.menih.adventofcode.lib.FileHelper

def toPairs (seq: Seq[Int]): Seq[(Int, Int)] =
  for i <- 0 until seq.size - 1
    yield (seq(i), seq(i + 1))

@main def Day01Part1(args: String*): Unit =
  val inputs: List[Int] = FileHelper.readInputInts("day01.txt")
  val solution = toPairs(inputs).count((a, b) => a < b)

  println(s"Solution: $solution")
