package si.menih.adventofcode
package Day8Part1

import si.menih.adventofcode.lib.FileHelper
import javax.swing.text.Position

@main def Day08(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day08.txt")

  val outputs = inputLines.map(_.split(" \\| ").last.split(" ")).flatten
  val uniqueOutputs = outputs.filter(l => List(2, 3, 4, 7).contains(l.length))

  println(s"Solution: ${uniqueOutputs.length}")

