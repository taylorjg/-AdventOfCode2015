import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val placements = parseLines(lines)
    println(placements)
    val part1Answer = part1(placements)
    println(s"part 1 answer: $part1Answer")
  }

  final case class Placement(person1: String,
                             person2: String,
                             happinessDelta: Int)

  private final val PlacementRegex =
    raw"""(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.""".r

  private def parseLines(lines: Seq[String]): Seq[Placement] =
    lines.map(parseLine)

  private def parseLine(line: String): Placement =
    line match {
      case PlacementRegex(p1, "gain", dh, p2) => Placement(p1, p2, +dh.toInt)
      case PlacementRegex(p1, "lose", dh, p2) => Placement(p1, p2, -dh.toInt)
      case _                                  => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def part1(placements: Seq[Placement]): Int =
    0
}
