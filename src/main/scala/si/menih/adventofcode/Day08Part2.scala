package si.menih.adventofcode
package Day8Part2

import si.menih.adventofcode.lib.FileHelper
import javax.swing.text.Position

object SevenSegmentSolver:
  def containsAll (s: String, c: String): Boolean = c.forall(s.contains(_))
  def containsN(s: String, c: String, n: Int): Boolean = c.count(s.contains(_)) == n

  def solveSignals(inputs: Seq[String]): Map[String, Int] =
    // Easy numbers
    val one = inputs.find(p => p.length == 2).get
    val four = inputs.find(p => p.length == 4).get
    val seven = inputs.find(p => p.length == 3).get
    val eight = inputs.find(p => p.length == 7).get

    val fiveLength = inputs.filter(_.length == 5)
    val sixLength = inputs.filter(_.length == 6)

    val three = fiveLength.find(containsAll(_, seven)).get
    val six = sixLength.find(containsN(_, one, 1)).get
    val nine = sixLength.find(containsAll(_, three)).get
    val zero = sixLength.find(v => v != six && v != nine).get
    val five = fiveLength.find(containsAll(six, _)).get
    val two = fiveLength.find(!List(five, three).contains(_)).get

    Map(
      one -> 1,
      two -> 2,
      three -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eight -> 8,
      nine -> 9,
      zero -> 0,
    )

  def solve(inputs: Seq[String], outputs: Seq[String]): Int =
    var numMap: Map[String, Int] = solveSignals(inputs)

    if (numMap.size != 10)
      println(inputs)
      println(numMap)
      throw new Exception("Missing numbers!")

    outputs.foldLeft(0) { (acc, output) =>
      val num = numMap(output)
      acc * 10 + num
    }

def splitInput (line: String): (Seq[String], Seq[String]) =
     line.split(" \\| ") match
        case Array(a: String, b: String) => (a.split(" ").toSeq, b.split(" ").toSeq)
        case _ => throw new Exception("Invalid input")

def sortCodes (inputs: Seq[String], outputs: Seq[String]): (Seq[String], Seq[String]) =
  (
    inputs.map(_.split("").sorted.mkString),
    outputs.map(_.split("").sorted.mkString),
  )

@main def Day08(args: String*): Unit =
  val inputLines: List[String] = FileHelper.readInputLines("day08.txt")
  val data: Seq[(Seq[String], Seq[String])] = inputLines.map(splitInput).map(sortCodes)
  val parsedOutputs = for ((inputs, outputs) <- data) yield SevenSegmentSolver.solve(inputs, outputs)

  println(s"Solution: ${parsedOutputs.sum}")
