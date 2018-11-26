import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val capacities = lines.map(_.toInt)
    val part1Answer = part1(capacities)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(capacities)
    println(s"part 2 answer: $part2Answer")
  }

  private def part1(capacities: List[Int]): Int = {
    val capacitiesWithIndices = capacities.zipWithIndex
    val allCombinationsWithIndices = (1 to capacitiesWithIndices.length).flatMap(capacitiesWithIndices.combinations)
    val allCombinations = allCombinationsWithIndices.map(xs => xs.map(_._1))
    val validCombinations = allCombinations.filter(_.sum == 150)
    validCombinations.length
  }

  private def part2(capacities: List[Int]): Int = {
    val capacitiesWithIndices = capacities.zipWithIndex
    val allCombinationsWithIndices = (1 to capacitiesWithIndices.length).flatMap(capacitiesWithIndices.combinations)
    val allCombinations = allCombinationsWithIndices.map(xs => xs.map(_._1))
    val validCombinations = allCombinations.filter(_.sum == 150)
    val minContainers = validCombinations.minBy(_.length).length
    validCombinations.count(_.length == minContainers)
  }
}
