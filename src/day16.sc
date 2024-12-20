import scala.io.Source
import pprint.pprintln

def readInput(s: String) = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .toVector

val rotateCost = 10000
val walkCost = 1

enum Direction:
  case Up, Right, Down, Left

extension (dir: Direction)
  def next = Direction.fromOrdinal((dir.ordinal + 1) % 4)
  def prev = Direction.fromOrdinal((dir.ordinal + 3) % 4)
  def opposite = Direction.fromOrdinal((dir.ordinal + 2) % 4)

type Cell = (Int, Int)
case class CellDir(cell: Cell, dir: Direction, weight: Int)
type Maze = Seq[Cell]
type WeightMap = Map[(Cell, Direction), Int]

extension (celldir: CellDir)
  def weightedNeighbours(weightMap: WeightMap): Seq[CellDir] =
    val neighbours = celldir match {
      case CellDir(cell, dir @ Direction.Down, weight) => List(
          celldir.copy(
            cell = (cell._1, cell._2 + 1),
            weight = weight + walkCost),
          celldir.copy(dir = dir.next, weight = weight + rotateCost),
          celldir.copy(dir = dir.prev, weight = weight + rotateCost),
          celldir.copy(dir = dir.opposite, weight = weight + rotateCost * 2)
        )
      case CellDir(cell, dir @ Direction.Up, weight) => List(
          celldir.copy(
            cell = (cell._1, cell._2 - 1),
            weight = weight + walkCost),
          celldir.copy(dir = dir.next, weight = weight + rotateCost),
          celldir.copy(dir = dir.prev, weight = weight + rotateCost),
          celldir.copy(dir = dir.opposite, weight = weight + rotateCost * 2)
        )
      case CellDir(cell, dir @ Direction.Left, weight) => List(
          celldir.copy(
            cell = (cell._1 - 1, cell._2),
            weight = weight + walkCost),
          celldir.copy(dir = dir.next, weight = weight + rotateCost),
          celldir.copy(dir = dir.prev, weight = weight + rotateCost),
          celldir.copy(dir = dir.opposite, weight = weight + rotateCost * 2)
        )
      case CellDir(cell, dir @ Direction.Right, weight) => List(
          celldir.copy(
            cell = (cell._1 + 1, cell._2),
            weight = weight + walkCost),
          celldir.copy(dir = dir.next, weight = weight + rotateCost),
          celldir.copy(dir = dir.prev, weight = weight + rotateCost),
          celldir.copy(dir = dir.opposite, weight = weight + rotateCost * 2)
        )
    }
    neighbours
      .filter(neigh =>
        weightMap.keys.toSeq.map(_._1).distinct.contains(neigh.cell))
end extension

extension (weightMap: WeightMap)
  def update(celldir: CellDir): WeightMap =
    val currentWeight =
      weightMap.getOrElse((celldir.cell, celldir.dir), Int.MaxValue)
    if currentWeight > celldir.weight
    then weightMap + ((celldir.cell, celldir.dir) -> celldir.weight)
    else weightMap

  def merge(other: WeightMap): WeightMap =
    weightMap ++
      other.filter((cellAndDir, weight) => weightMap(cellAndDir) > weight)
end extension

def parseMaze(input: Seq[String]): Maze =
  for
    x <- 0 until input(0).size
    y <- 0 until input.size
    if Seq('.', 'S', 'E').contains(input(y)(x))
  yield (x, y)

def applyAlgorithm(maze: Maze, startCell: Cell): WeightMap = {
  val weightMap = maze
    .flatMap(cell => Direction.values.map(dir => (cell, dir) -> Int.MaxValue))
    .toMap
  // pprintln(weightMap)
  pprintln(Direction.Down.prev)

  dfs(weightMap, CellDir(startCell, Direction.Right, 0))

  ???
}

def dfs(weightMap: WeightMap, currentCell: CellDir): WeightMap = {
  val newWeightMap = currentCell
    .weightedNeighbours(weightMap)
    .foldLeft(weightMap)((weightM, celldir) => weightM.update(celldir))

  pprintln(currentCell
    .weightedNeighbours(weightMap)
    .map(neighbour => newWeightMap((neighbour.cell, neighbour.dir))))

  val nWeightMap = currentCell
    .weightedNeighbours(weightMap)
    .filter { case CellDir(cell, dir, weight) =>
      weightMap((cell, dir)) > weight
    }
    .map(neigh => dfs(weightMap.update(neigh), neigh))
    .fold(weightMap)((a, b) => a.merge(b))

  if nWeightMap.equals(weightMap)
  then weightMap
  else nWeightMap
}

def part1() = {
  val input = readInput("./examples/day16-1.txt")
  val maze = parseMaze(input)
  val startCell = maze.find(cell => input(cell._2)(cell._1) == 'S').get
  val endCell = maze.find(cell => input(cell._2)(cell._1) == 'E').get
  applyAlgorithm(maze, startCell)
  // pprintln(maze)
}

part1()
