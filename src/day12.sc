import scala.io.Source
import pprint.pprintln

type Garden = Map[(Int, Int), Char]
type Plot = (Int, Int)

def readInput(s: String): Garden = Source
  .fromFile(s)
  .getLines
  .filterNot(_.isBlank)
  .zipWithIndex
  .flatMap { case (row, rowIdx) =>
    row.zipWithIndex.map {
      case (plant, colIdx) => ((colIdx, rowIdx), plant)
    }
  }
  .toMap

extension (plot: Plot)
  def neighbours = List(
    (plot._1 + 1, plot._2),
    (plot._1 - 1, plot._2),
    (plot._1, plot._2 + 1),
    (plot._1, plot._2 - 1)
  )

extension (garden: Garden)
  def dimensions: (Int, Int) = {
    val x = garden.keys.max
    (x._1 + 1, x._2 + 1)
  }

  def findRegions(unvisited: Set[Plot]): Seq[Set[Plot]] =
    unvisited.headOption match
      case None => List()
      case Some(startingPlot) => {
        val currentRegion = expandRegion(Set(startingPlot))
        findRegions(unvisited -- currentRegion) :+ currentRegion
      }

  def expandRegion(plotsInRegion: Set[Plot]): Set[Plot] =
    val plotType = garden(plotsInRegion.head)
    val expandedPlots = plotsInRegion
      .flatMap(_.neighbours.filter(plot =>
        garden.getOrElse(plot, '!') == plotType).toSet)
      .concat(plotsInRegion)
    if expandedPlots == plotsInRegion
    then plotsInRegion
    else expandRegion(expandedPlots)

type Region = Set[Plot]

extension (region: Region)
  def area = region.size
  def perimeter = region.map()

def part1() = {
  val garden = readInput("./examples/day12-1.txt")
  val (width, height) = garden.dimensions
  val unvisitedSeq = for {
    x <- 0 until width
    y <- 0 until height
  } yield (x, y)
  val unvisited = unvisitedSeq.toSet
  // pprintln(unvisited)

  val regions = garden.findRegions(unvisited)

  pprintln(garden.dimensions)
  pprintln(regions)
  // pprintln(garden)
}

part1()
