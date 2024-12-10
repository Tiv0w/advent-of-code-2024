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
): Int = {
  val startHeight = map(start)
  val neighbours = List(
    (start._1 + 1, start._2),
    (start._1 - 1, start._2),
    (start._1, start._2 + 1),
    (start._1, start._2 - 1)
  )
  val validNeighbours = neighbours
    .filter(neighbour => map.getOrElse(neighbour, -1) == startHeight + 1)

  validNeighbours.map(neighbour => followTrailhead(map, neighbour))

  pprintln(validNeighbours)

  ???
}

def part1() = {
  val input = readInput("./examples/day10.txt")
  val map: Map[(Int, Int), Int] = parseInput(input)
  given mapDim: (Int, Int) = (input(0).size, input.size)
  val result = followTrailhead(map, (0, 0))
  pprintln(result)
}

part1()
