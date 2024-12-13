import scala.io.Source
import pprint.pprintln


def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector

def part1() = {
  val input = readInput("./examples/day13-1.txt")
  pprintln(input)
}

part1()
