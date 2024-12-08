import scala.io.Source
import pprint.pprintln
import scala.util.parsing.combinator.*
import scala.util.matching.Regex.Match
import scala.util.chaining.*

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector

object MulParser extends RegexParsers {
  def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def chars: Parser[String] = """.+""".r ^^ { _.toString }
  def mul: Parser[Int] = "mul(" ~ number ~ "," ~ number ~ ")" ^^ {
    case _ ~ a ~ _ ~ b ~ _ => a * b
  }
  // def mulInChars = chars ~> mul ~ mulInChars ^^
}

val mulRegex = raw"mul\((\d+),(\d+)\)".r

def part1() = {
  val input = readInput("./examples/day3.txt")
  val result = input
    .map(x =>
      mulRegex
        .findAllMatchIn(x)
        .toList
        .map(_.subgroups.map(_.toInt).reduce(_ * _))
        .sum)
    .sum

  // val result = MulParser.parseAll(MulParser.mul, test)

  pprintln(result)
}

part1()

val mulDoDontRegex = raw"do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)".r

def part2() = {
  val input = readInput("./input/day3.txt")
  val result = input
    // .take(2)
    .map(x =>
      mulDoDontRegex
        .findAllMatchIn(x)
        .toList
        .tap(println)
        .foldLeft[(Boolean, Int)]((true, 0)) {
          case ((_, acc), Match("do()")) =>
            (true, acc)
          case ((_, acc), Match("don't()")) =>
            (false, acc)
          case ((true, acc), m) =>
            (true, acc + m.subgroups.map(_.toInt).reduce(_ * _))
          case ((false, acc), _) =>
            (false, acc)
        }
        ._2
    // .map(_.subgroups.map(_.toInt).reduce(_ * _))
    // .sum

    )
  // .sum

  // val result = MulParser.parseAll(MulParser.mul, test)

  pprintln(result)
  pprintln(result.sum)
}

part2()
