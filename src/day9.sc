import scala.io.Source
import pprint.pprintln

def expandLayout(diskMap: String): String =
  diskMap
    .zipWithIndex
    .foldLeft("") { case (acc, (c, idx)) =>
      val idNumber = idx / 2
      val chosenChar = if idx % 2 == 1 then "." else idNumber.toString
      try
        acc + chosenChar.repeat(c.asDigit)
      catch {
        case e => pprintln(e); pprintln(c); ""
      }
    }

val freeSpaceBeforeFile = raw"\d+(\.)\.*\d+".r
// val freeSpace = raw"\d+(\.)?\.*\d+".r

def moveBlocks(expanded: String): String = {
  // pprintln(expanded)
  freeSpaceBeforeFile.findFirstMatchIn(expanded) match {
    case Some(m) => {
      // pprintln(m);
      val firstFreeSpaceIdx = m.start(1)
      val lastDigitIdx = expanded.lastIndexWhere(_.isDigit)
      val lastDigit = expanded(lastDigitIdx)
      moveBlocks(expanded
        .updated(firstFreeSpaceIdx, lastDigit)
        .updated(lastDigitIdx, '.'))
    }
    case None => expanded
  }
}

def checksum(finalExpansion: String): BigInt =
  finalExpansion
    .filter(_.isDigit)
    .zipWithIndex
    .map { case (digit, idx) => BigInt(digit.asDigit) * BigInt(idx) }
    .sum

def part1() = {
  val input = Source.fromFile("./input/day9.txt").mkString.trim
  // val input = "12345"

  val result = checksum(moveBlocks(expandLayout(input)))
  // pprintln(moveBlocks(result))

  pprintln(result)
}

part1()
