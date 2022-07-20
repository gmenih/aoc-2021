package si.menih
package adventofcode
package Day1Part2

import si.menih.adventofcode.lib.FileHelper

def toPairs (seq: Seq[Int]): Seq[(Int, Int)] =
  for i <- 0 until seq.size - 1
    yield (seq(i), seq(i + 1))

@main def Day01Part2(args: String*): Unit =
  val inputs: List[Int] = FileHelper.readInputInts("day01.txt")

  val windows: Seq[Int] = inputs
    .sliding(3)
    .map[Int]({
      case List(a, b, c) => (a + b + c)
      case _ => throw new IllegalArgumentException("List must have 3 elements")
    })
    .toSeq


  val solution = toPairs(windows).count((a, b) => a < b)

  println(s"Solution: $solution")
