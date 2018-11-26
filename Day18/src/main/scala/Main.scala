import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val fileName = "test.txt" // "input.txt"
    val numSteps = 4 // 100
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
    initialGrid.foreach(println)
    val finalGrid = Iterable.iterate(initialGrid, numSteps + 1)(step).last
    countOnLights(finalGrid)
  }

  private def step(grid: Grid): Grid = {
    // TODO: implement this properly
    grid
  }

  private def countOnLights(grid: Grid): Int =
    grid.map(gridRow => gridRow.count(identity)).sum
}
