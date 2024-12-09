import scala.io.Source
import pprint.pprintln
import scala.annotation.tailrec

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector

def parseInput(i: Seq[String]) = i
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

def isOutOfMap(position: (Int, Int))(using mapDimensions: (Int, Int)) =
  position._1 < 0
    || position._2 < 0
    || position._1 >= mapDimensions._1
    || position._2 >= mapDimensions._2

def getNextDirection(prev: Direction): Direction =
  prev match {
    case Direction.Down  => Direction.Left
    case Direction.Left  => Direction.Up
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
  }

@tailrec
def stepSimulation(
    gameMap: GameMap,
    position: (Int, Int),
    direction: Direction,
    accumulator: Seq[(Int, Int)]
)(using mapDimensions: (Int, Int)): Seq[(Int, Int)] = {
  val nextCellCoords = direction match {
    case Direction.Down  => (position._1, position._2 + 1)
    case Direction.Up    => (position._1, position._2 - 1)
    case Direction.Left  => (position._1 - 1, position._2)
    case Direction.Right => (position._1 + 1, position._2)
  }
  if isOutOfMap(nextCellCoords)
  then
    accumulator
  else if gameMap(nextCellCoords) == '#'
  then
    stepSimulation(
      gameMap,
      position,
      getNextDirection(direction),
      accumulator)
  else
    stepSimulation(
      gameMap,
      nextCellCoords,
      direction,
      accumulator :+ nextCellCoords)
}

def part1() = {
  val input = readInput("./input/day6.txt")
  given mapDimensions: (Int, Int) = (input(0).size, input.size)
  val gameMap: GameMap = parseInput(input)
  val startingPosition = gameMap.find(_._2 == '^').get._1
  val result = stepSimulation(
    gameMap,
    startingPosition,
    Direction.Up,
    Vector(startingPosition))
    .reverse
    .distinct

  pprintln(result.size)
}

part1()

/** Part 2
  */

@tailrec
def stepSimulationPart2(
    gameMap: GameMap,
    position: (Int, Int),
    direction: Direction,
    previousStates: Set[((Int, Int), Direction)]
)(using mapDimensions: (Int, Int)): Boolean = {
  val nextCellCoords = direction match {
    case Direction.Down  => (position._1, position._2 + 1)
    case Direction.Up    => (position._1, position._2 - 1)
    case Direction.Left  => (position._1 - 1, position._2)
    case Direction.Right => (position._1 + 1, position._2)
  }
  if isOutOfMap(nextCellCoords)
  then
    false
  else if previousStates.contains((position, direction))
  then
    true
  else if gameMap(nextCellCoords) == '#'
  then
    stepSimulationPart2(
      gameMap,
      position,
      getNextDirection(direction),
      previousStates + ((position, direction)))
  else
    stepSimulationPart2(
      gameMap,
      nextCellCoords,
      direction,
      previousStates + ((position, direction)))
}

def part2() = {
  val input = readInput("./input/day6.txt")
  given mapDimensions: (Int, Int) = (input(0).size, input.size)
  val gameMap: GameMap = parseInput(input)
  val startingPosition = gameMap.find(_._2 == '^').get._1

  val obstructionPlaces = for {
    x <- 0 until mapDimensions._1
    y <- 0 until mapDimensions._2
    if gameMap((x, y)) == '.'
  } yield (x, y)

  val result = obstructionPlaces
    .count(obstruction =>
      stepSimulationPart2(
        gameMap.updated(obstruction, '#'),
        startingPosition,
        Direction.Up,
        Set()))

  pprintln(result)
}

part2()
