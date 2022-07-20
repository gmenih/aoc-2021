package si.menih.adventofcode.lib

import scala.io.Source

object FileHelper:

  def readInputLines (fileName: String): List[String] =
    Source.fromFile(s"./src/inputs/$fileName").getLines.toList

  def readInputInts: String => List[Int] = readInputLines(_).map(_.toInt)
