import scala.io.Source
import pprint.pprintln

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector
  .map { line =>
    val Array(testValue, numbers) = line.split(": ")
    (testValue.toInt, numbers.split(" ").map(_.toInt).toList)
  }

val add = new Function2[Int, Int, Int] {
  override def apply(v1: Int, v2: Int): Int = v1 + v2
  override def toString(): String = "+"
}

val mul = new Function2[Int, Int, Int] {
  override def apply(v1: Int, v2: Int): Int = v1 * v2
  override def toString(): String = "*"
}

def countCombinations(target: Int, numbers: Seq[Int]): Int = {
  val operators = Array(add, mul)
  pprintln(operators.combinations(numbers.size - 1).toList)
  // numbers.sl
  1
}

def part1() = {
  val input = readInput("./examples/day7.txt")
  countCombinations(input(0)._1, input(0)._2)
  // pprintln(input)
}

part1()
