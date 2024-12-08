import scala.io.Source
import pprint.pprintln
import scala.util.chaining.*

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector

type IndexedMatrix = Map[(Int, Int), Char]

def computeIndexedMatrix(s: Seq[String]): Map[(Int, Int), Char] = s
  .zipWithIndex
  .flatMap {
    case (row, rowIdx) => row.zipWithIndex.map {
        case (char, colIdx) => ((colIdx, rowIdx), char)
      }
  }
  .toMap

def computeNeighbours(coords: (Int, Int))(using
    matrixDim: (Int, Int)
): Seq[Seq[(Int, Int)]] = {
  val allNeighboursCoords =
    for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x == 0 && y == 0)
    } yield (0 to 3).map(m => (x * m, y * m))

  // map the neighboursCoords to the real case, then filter out impossible ones
  val realNeighboursCoords = allNeighboursCoords
    .map(_.map(v => (v._1 + coords._1, v._2 + coords._2)))
    .filter(_.forall {
      case (x, y) =>
        (0 <= x && x < matrixDim._1) && (0 <= y && y < matrixDim._2)
    })

  realNeighboursCoords
}

def mapPossibilityToChars(
    m: IndexedMatrix,
    possibility: Seq[(Int, Int)]
): String =
  possibility.map(coords => m(coords)).mkString

def part1() = {
  val input = readInput("./input/day4.txt")
  given matrixDim: (Int, Int) = (input(0).size, input.size)

  val indexedMatrix = computeIndexedMatrix(input)

  val allCells = for {
    x <- 0 until input(0).size
    y <- 0 until input.size
  } yield (x, y)

  val result = allCells
    .map(computeNeighbours)
    .flatten
    .map(possibility => mapPossibilityToChars(indexedMatrix, possibility))
    .count(_ == "XMAS")

  pprintln(result)
}

part1()

/*
Part 2
 *
 */

def computeNeighboursPart2(coords: (Int, Int))(using
    matrixDim: (Int, Int)
): Seq[Seq[(Int, Int)]] = {
  val allNeighboursCoords =
    Vector(
      Vector((-1, -1), (0, 0), (1, 1)),
      Vector((-1, 1), (0, 0), (1, -1)),
      Vector((1, -1), (0, 0), (-1, 1)),
      Vector((1, 1), (0, 0), (-1, -1))
    )

  // map the neighboursCoords to the real case, then filter out impossible ones
  val realNeighboursCoords = allNeighboursCoords
    .map(_.map(v => (v._1 + coords._1, v._2 + coords._2)))
    .filter(_.forall {
      case (x, y) =>
        (0 <= x && x < matrixDim._1) && (0 <= y && y < matrixDim._2)
    })

  realNeighboursCoords
}

def part2() = {
  val input = readInput("./input/day4.txt")
  given matrixDim: (Int, Int) = (input(0).size, input.size)

  val indexedMatrix = computeIndexedMatrix(input)

  val allCells = for {
    x <- 0 until input(0).size
    y <- 0 until input.size
  } yield (x, y)

  val result = allCells
    .map(computeNeighboursPart2)
    .map(_.map(possibility =>
      mapPossibilityToChars(indexedMatrix, possibility)))
    .filter(_.count(_ == "MAS") == 2)
    .size

  pprintln(result)
}

part2()
