package si.menih
package adventofcode
package Day3Part2

import si.menih.adventofcode.lib.FileHelper
import scala.annotation.tailrec

type BitList = Seq[1 | 0];
type Predicate = (a: Int, b: Int) => Boolean;

def bitListToDecimal(l: BitList): Int =
  l.foldLeft(0)((a, b) => a * 2 + b)

def countBitOccurrences(bits: Seq[Seq[Int]], position: Int): Int =
  bits.map(v => v(position)).sum

@main def Day03Part2(args: String*): Unit =
  val inputs: List[String] = FileHelper.readInputLines("day03.txt")

  val bits: List[BitList] =
    inputs.map(i => i.iterator.map[1 | 0](v => if v - '0' == 0 then 0 else 1).toSeq).toSeq

  @tailrec def reduceBits(list: Seq[BitList], cmp: Predicate, i: Int = 0): BitList =
    val occurrences = countBitOccurrences(list, i)
    val commonBit = if cmp(occurrences, list.length) then 1 else 0
    val remaining = list.filter(b => b(i) == commonBit)

    if remaining.length <= 1 then remaining.head
    else reduceBits(remaining, cmp, i + 1)

  def oxygenPredicate: Predicate = (o, l) => o >= (l / 2 + l % 2)
  def co2Predicate: Predicate = (o, l) => o < (l / 2)

  val oxygen = bitListToDecimal(reduceBits(bits, oxygenPredicate))
  val co2 = bitListToDecimal(reduceBits(bits, co2Predicate))

  println(s"The solution is ${oxygen * co2}")
