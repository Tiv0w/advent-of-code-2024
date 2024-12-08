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

def isOutOfMap(gameMap: GameMap, position: (Int, Int)) =
  position._1 < 0
    || position._2 < 0
    || position._1 >= gameMap.keys.map(_._1).max
    || position._2 >= gameMap.keys.map(_._2).max

def getNextDirection(prev: Direction): Direction =
  prev match {
    case Direction.Down  => Direction.Left
    case Direction.Left  => Direction.Up
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
  }

def stepSimulation(
    gameMap: GameMap,
    position: (Int, Int),
    direction: Direction
): Seq[(Int, Int)] = {
  val nextCellCoords = direction match {
    case Direction.Down  => (position._1, position._2 + 1)
    case Direction.Up    => (position._1, position._2 - 1)
    case Direction.Left  => (position._1 - 1, position._2)
    case Direction.Right => (position._1 + 1, position._2)
  }
  if isOutOfMap(gameMap, position)
  then Seq()
  else if gameMap(nextCellCoords) == '#'
  then
    stepSimulation(
      gameMap,
      position,
      getNextDirection(direction)) :+ nextCellCoords
  else
    stepSimulation(
      gameMap,
      nextCellCoords,
      direction) :+ nextCellCoords
}

def part1() = {
  val gameMap: GameMap = readInput("./examples/day6.txt")
  val startingPosition = gameMap.find(_._2 == '^').get._1
  pprintln(startingPosition)
  val result = stepSimulation(gameMap, startingPosition, Direction.Up)
    .distinct

  pprintln(result.size)
}

part1()
