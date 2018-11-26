import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    // val fileName = "test.txt"
    // val numSteps = 4
    val fileName = "input.txt"
    val numSteps = 100
    val lines = Source.fromResource(fileName).getLines().toList
    val grid = parseLines(lines)
    val part1Answer = part1(grid, numSteps)
    println(s"part 1 answer: $part1Answer")
  }

  type GridRow = Vector[Boolean]
  type Grid = Vector[GridRow]

  private def parseLines(lines: Seq[String]): Grid =
    lines.toVector.map(parseLine)

  private def parseLine(line: String): GridRow =
    line.toVector.map(_ == '#')

  private def part1(initialGrid: Grid, numSteps: Int): Int = {
    val finalGrid = Iterable.iterate(initialGrid, numSteps + 1)(step).last
    countOnLights(finalGrid)
  }

  private def step(grid: Grid): Grid = {
    val numRows = grid.length
    val numCols = grid.head.length
    Vector.tabulate(numRows, numCols)(newState(grid))
  }

  private def newState(grid: Grid)(row: Int, col: Int): Boolean = {
    val isOn = grid(row)(col)
    val numOnNeighbours = countOnNeighbours(grid, row, col)
    numOnNeighbours match {
      case 2 | 3 if isOn => true
      case _ if isOn     => false
      case 3 if !isOn    => true
      case _             => false
    }
  }

  private def countOnNeighbours(grid: Grid, row: Int, col: Int): Int = {
    val maxRowBound = grid.indices.last
    val maxColBound = grid.head.indices.last
    val coords = for {
      r <- row - 1 to row + 1
      c <- col - 1 to col + 1
      if r != row || c != col
      if r >= 0 && r <= maxRowBound && c >= 0 && c <= maxColBound
    } yield (r, c)
    val values = coords.map { case (r, c) => grid(r)(c) }
    values.count(identity)
  }

  private def countOnLights(grid: Grid): Int =
    grid.map(gridRow => gridRow.count(identity)).sum

  // private def dumpGrid(grid: Grid): Unit =
  //   grid.foreach(gridRow => println(gridRow.map(if (_) '#' else '.').mkString))
}
