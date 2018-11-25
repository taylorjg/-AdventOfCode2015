import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val aunts = parseLines(lines)
    val part1Answer = part1(aunts)
    println(s"part 1 answer: $part1Answer")
  }

  private final val LineRegex =
    raw"""Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)""".r

  type Item = (String, Int)
  final case class Aunt(number: Int, items: List[Item])

  private def parseLines(lines: Seq[String]): Seq[Aunt] =
    lines.map(parseLine)

  private def parseLine(line: String): Aunt =
    line match {
      case LineRegex(number, t1, q1, t2, q2, t3, q3) =>
        Aunt(number.toInt, List(t1 -> q1.toInt, t2 -> q2.toInt, t3 -> q3.toInt))
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def part1(aunts: Seq[Aunt]): Int = {
    aunts.foreach(println)
    0
  }
}
