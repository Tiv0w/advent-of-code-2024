import scala.io.Source
import pprint.pprintln

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector
  .zipWithIndex
  .map { case (row, rowIdx) =>
    row.zipWithIndex.map {
      case (cell, colIdx) => ((colIdx, rowIdx), cell)
    }
  }
  .flatten
  .toMap

type GameMap = Map[(Int, Int), Char]
enum Direction:
  case Up, Down, Left, Right

def stepSimulation(
    gameMap: GameMap,
    position: (Int, Int),
    direction: Direction
): (Int, Int) =
  direction match {
    case Direction.Down  => position
    case Direction.Up    => position
    case Direction.Left  => position
    case Direction.Right => position
  }

def part1() = {
  val gameMap: GameMap = readInput("./examples/day6.txt")
  val startingPosition = gameMap.find(_._2 == '^').get._1
  pprintln(startingPosition)
  val result = gameMap

  pprintln(result)
}

part1()
