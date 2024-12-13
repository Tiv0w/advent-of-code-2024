import scala.io.Source
import pprint.pprintln

def readInput(s: String) =
  Source
    .fromFile(s)
    .getLines
    .filterNot(_.isBlank)
    .map(_.toVector)
    .toVector

def parseInput(input: Seq[Seq[Char]]): Map[(Int, Int), Int] =
  input
    .zipWithIndex
    .map { case (row, rowIdx) =>
      row.zipWithIndex.map {
        case (height, colIdx) => ((colIdx, rowIdx), height.asDigit)
      }
    }
    .flatten
    .toMap

def followTrailhead(map: Map[(Int, Int), Int], start: (Int, Int))(using
    mapDim: (Int, Int)
): Seq[(Int, Int)] = {
  val startHeight = map(start)
  if (startHeight == 9) {
    List(start)
  } else {
    val neighbours = List(
      (start._1 + 1, start._2),
      (start._1 - 1, start._2),
      (start._1, start._2 + 1),
      (start._1, start._2 - 1)
    )
    val validNeighbours = neighbours
      .filter(neighbour => map.getOrElse(neighbour, -1) == startHeight + 1)

    validNeighbours
      .map(neighbour => followTrailhead(map, neighbour))
      .flatten
      .distinct
  }
}

def part1() = {
  val input = readInput("./input/day10.txt")
  val map: Map[(Int, Int), Int] = parseInput(input)
  given mapDim: (Int, Int) = (input(0).size, input.size)
  val startingTrailheads = map.iterator.filter(_._2 == 0).map(_._1).toList

  val result = startingTrailheads
    .map(trailhead => followTrailhead(map, trailhead).size)
    .sum
  pprintln(result)
}

part1()

/** Part 2
  */

def followTrailheadPart2(map: Map[(Int, Int), Int], start: (Int, Int))(using
    mapDim: (Int, Int)
): Int = {
  val startHeight = map(start)
  if (startHeight == 9) {
    1
  } else {
    val neighbours = List(
      (start._1 + 1, start._2),
      (start._1 - 1, start._2),
      (start._1, start._2 + 1),
      (start._1, start._2 - 1)
    )
    val validNeighbours = neighbours
      .filter(neighbour => map.getOrElse(neighbour, -1) == startHeight + 1)

    validNeighbours.map(neighbour => followTrailheadPart2(map, neighbour)).sum
  }
}

def part2() = {
  val input = readInput("./input/day10.txt")
  val map: Map[(Int, Int), Int] = parseInput(input)
  given mapDim: (Int, Int) = (input(0).size, input.size)
  val startingTrailheads = map.iterator.filter(_._2 == 0).map(_._1).toList

  val result = startingTrailheads
    .map(trailhead => followTrailheadPart2(map, trailhead))
    .sum
  pprintln(result)
}

part2()
