import scala.io.Source
import pprint.pprintln

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector
  .map(_.split(" ").map(_.toInt))

def calculateLevelsDiffs(report: Array[Int]): List[Int] =
  report.sliding(2).map(_.reduce(_ - _)).toList

def isReportOk(report: Seq[Int]): Boolean =
  report.forall(x => -3 <= x && x <= -1) || report.forall(x => 1 <= x && x <= 3)

def part1() = {
  val input = readInput("./input/day2.txt")
  val result = input
    .map(calculateLevelsDiffs)
    .filter(isReportOk)

  pprintln(result.size)
}

part1()

def part2() = {
  val input = readInput("./input/day2.txt")
  val result = input
    .map(x => x.combinations(x.size - 1).toList :+ x)
    .map(_.map(calculateLevelsDiffs))
    .count(_.exists(isReportOk))

  pprintln(result)
}

part2()
