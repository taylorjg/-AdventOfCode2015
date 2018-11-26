import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val grid = parseLines(lines)
    val numSteps = 100
    val part1Answer = runSteps(grid, numSteps, part2 = false)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = runSteps(grid, numSteps, part2 = true)
    println(s"part 2 answer: $part2Answer")
  }

  type GridRow = Vector[Boolean]
  type Grid = Vector[GridRow]

  private def parseLines(lines: Seq[String]): Grid =
    lines.toVector.map(parseLine)

  private def parseLine(line: String): GridRow =
    line.toVector.map(_ == '#')

  private def runSteps(initialGrid: Grid,
                       numSteps: Int,
                       part2: Boolean): Int = {
    val finalGrid =
      Iterable.iterate(initialGrid, numSteps + 1)(step(part2)).last
    countOnLights(finalGrid)
  }

  private def step(part2: Boolean)(grid: Grid): Grid = {
    val numRows = grid.length
    val numCols = grid.head.length
    Vector.tabulate(numRows, numCols)(newState(grid, part2))
  }

  private def newState(grid: Grid, part2: Boolean)(row: Int,
                                                   col: Int): Boolean = {
    val maxRowBound = grid.indices.last
    val maxColBound = grid.head.indices.last
    val isCorner =
      (row == 0 && col == 0) ||
        (row == 0 && col == maxColBound) ||
        (row == maxRowBound && col == 0) ||
        (row == maxRowBound && col == maxColBound)
    if (part2 && isCorner) true
    else {
      val isOn = grid(row)(col)
      val numOnNeighbours = countOnNeighbours(grid, row, col, part2)
      numOnNeighbours match {
        case 2 | 3 if isOn => true
        case _ if isOn     => false
        case 3 if !isOn    => true
        case _             => false
      }
    }
  }

  private def countOnNeighbours(grid: Grid,
                                row: Int,
                                col: Int,
                                part2: Boolean): Int = {
    val maxRowBound = grid.indices.last
    val maxColBound = grid.head.indices.last
    def isCorner(r: Int, c: Int): Boolean =
      (r == 0 && c == 0) ||
        (r == 0 && c == maxColBound) ||
        (r == maxRowBound && c == 0) ||
        (r == maxRowBound && c == maxColBound)
    val coords = for {
      r <- row - 1 to row + 1
      c <- col - 1 to col + 1
      if r != row || c != col
      if r >= 0 && r <= maxRowBound && c >= 0 && c <= maxColBound
    } yield (r, c)
    val values = coords.map {
      case (r, c) =>
        if (part2 && isCorner(r, c)) true
        else grid(r)(c)
    }
    values.count(identity)
  }

  private def countOnLights(grid: Grid): Int =
    grid.map(gridRow => gridRow.count(identity)).sum
}
