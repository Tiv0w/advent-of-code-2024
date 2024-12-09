import scala.io.Source
import pprint.pprintln

def expandLayout(diskMap: String): String =
  diskMap
    .zipWithIndex
    .foldLeft("") { case (acc, (c, idx)) =>
      val idNumber = idx / 2
      val chosenChar = if idx % 2 == 1 then "." else idNumber.toString
      acc + chosenChar.repeat(c.asDigit)
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

def part1() = {
  // val input = Source.fromFile("./examples/day9.txt").mkString
  val input = "12345"

  val result = moveBlocks(expandLayout(input))
  // pprintln(moveBlocks(result))

  pprintln(result)
}

part1()
