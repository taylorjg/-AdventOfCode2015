import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toVector
    val instructions = parseLines(lines)
    val part1Answer = part1(instructions)
    println(s"part 1 answer: $part1Answer")
  }

  private final val HlfRegex = raw"""hlf (a|b)""".r
  private final val TplRegex = raw"""tpl (a|b)""".r
  private final val IncRegex = raw"""inc (a|b)""".r
  private final val JmpRegex = raw"""jmp ([+-]\d+)""".r
  private final val JieRegex = raw"""jie (a|b), ([+-]\d+)""".r
  private final val JioRegex = raw"""jio (a|b), ([+-]\d+)""".r

  sealed abstract class Instruction
  final case class Hlf(r: String) extends Instruction
  final case class Tpl(r: String) extends Instruction
  final case class Inc(r: String) extends Instruction
  final case class Jmp(offset: Int) extends Instruction
  final case class Jie(r: String, offset: Int) extends Instruction
  final case class Jio(r: String, offset: Int) extends Instruction

  private def parseLines(lines: Vector[String]): Vector[Instruction] =
    lines.map(parseLine)

  private def parseLine(line: String): Instruction =
    line match {
      case HlfRegex(r)         => Hlf(r)
      case TplRegex(r)         => Tpl(r)
      case IncRegex(r)         => Inc(r)
      case JmpRegex(offset)    => Jmp(offset.toInt)
      case JieRegex(r, offset) => Jie(r, offset.toInt)
      case JioRegex(r, offset) => Jio(r, offset.toInt)
      case _                   => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def part1(instructions: Vector[Instruction]): Int = {
    instructions.foreach(println)
    0
  }
}
