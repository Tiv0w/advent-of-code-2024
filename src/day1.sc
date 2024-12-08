import scala.io.Source
import pprint.pprintln

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector
  .map(_.split("\\s+").map(_.toInt))

def part1() = {
  val input = readInput("./examples/day1.txt")
  val result = input
    .transpose
    .map(_.sorted)
    .transpose
    .map(x => (x(0) - x(1)).abs)

  pprintln(result.sum)
}

part1()

def part2() = {
  val input = readInput("./examples/day1.txt")
    .transpose
  val firstList = input(0)
  val secondList = input(1)
  val counter = secondList.groupBy(identity).mapValues(_.size).toMap

  val result = firstList.map(x => x * counter.getOrElse(x, 0)).sum

  pprintln(result)
}

part2()
