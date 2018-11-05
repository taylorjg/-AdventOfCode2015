import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toSeq
    val instructions = parseLines(lines)
    part1(instructions)
  }

  sealed abstract class Disposition
  final case object TurnOn extends Disposition
  final case object TurnOff extends Disposition
  final case object Toggle extends Disposition

  final case class Coords(x: Int, y: Int)
  final case class Rect(corner1: Coords, corner2: Coords)
  final case class Instruction(disposition: Disposition, rect: Rect)

  private def parseLines(lines: Seq[String]): Seq[Instruction] =
    lines.map(parseLine)

  private final val LineRegex = """^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$""".r

  private def stringToDisposition(s: String): Disposition =
    s match {
      case "turn on" => TurnOn
      case "turn off" => TurnOff
      case "toggle" => Toggle
      case _ => throw new Exception(s"Failed to convert '$d' to a Disposition.")
    }

  private def parseLine(line: String): Instruction = {
    line match {
      case LineRegex(d, x1, y1, x2, y2) =>
        val disposition = stringToDisposition(d)
        val corner1 = Coords(x1.toInt, y1.toInt)
        val corner2 = Coords(x2.toInt, y2.toInt)
        val rect = Rect(corner1, corner2)
        Instruction(disposition, rect)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }
  }

  private def part1(instructions: Seq[Instruction]): Unit = {
    val answer = 0
    println(s"part 1 answer: $answer")
  }
}
