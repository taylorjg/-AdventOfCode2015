import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val instructions = parseLines(lines)
    println(instructions)
    part1(instructions)
  }

  sealed abstract class Source
  final case class Constant(value: Int) extends Source
  final case class Identifier(name: String) extends Source

  final case class Wire(name: String)

  sealed abstract class Instruction
  final case class Load(source: Source, wire: Wire) extends Instruction
  final case class And(source1: Source, source2: Source, wire: Wire) extends Instruction
  final case class Or(source1: Source, source2: Source, wire: Wire) extends Instruction
  final case class Not(source: Source, wire: Wire) extends Instruction
  final case class LeftShift(source: Source, n: Int, wire: Wire) extends Instruction
  final case class RightShift(source: Source, n: Int, wire: Wire) extends Instruction

  private def parseLines(lines: Seq[String]): Seq[Instruction] =
    lines.map(parseLine)

  private final val LoadConstantRegex = """^(\d+) -> ([a-z]+)$""".r
  private final val LoadIdRegex = """^([a-z]+) -> ([a-z]+)$""".r
  private final val AndConstantIdRegex = """^(\d+) AND ([a-z]+) -> ([a-z]+)$""".r
  private final val AndIdIdRegex4 = """^([a-z]+) AND ([a-z]+) -> ([a-z]+)$""".r
  private final val OrConstantIdRegex = """^(\d+) OR ([a-z]+) -> ([a-z]+)$""".r
  private final val OrIdIdRegex = """^([a-z]+) OR ([a-z]+) -> ([a-z]+)$""".r
  private final val NotConstantRegex = """^NOT (\d+) -> ([a-z]+)$""".r
  private final val NotIdRegex = """^NOT ([a-z]+) -> ([a-z]+)$""".r
  private final val LeftShiftConstantRegex = """^(\d+) LSHIFT (\d+) -> ([a-z]+)$""".r
  private final val LeftShiftIdRegex = """^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$""".r
  private final val RightShiftConstantRegex = """^(\d+) RSHIFT (\d+) -> ([a-z]+)$""".r
  private final val RightShiftIdRegex = """^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$""".r

  private def parseLine(line: String): Instruction = {
    line match {

      case LoadConstantRegex(v, w) =>
        Load(Constant(v.toInt), Wire(w))

      case LoadIdRegex(id, w) =>
        Load(Identifier(id), Wire(w))

      case AndConstantIdRegex(v, id, w) =>
        And(Constant(v.toInt), Identifier(id), Wire(w))

      case AndIdIdRegex4(id1, id2, w) =>
        And(Identifier(id1), Identifier(id2), Wire(w))

      case OrConstantIdRegex(v, id, w) =>
        Or(Constant(v.toInt), Identifier(id), Wire(w))

      case OrIdIdRegex(id1, id2, w) =>
        Or(Identifier(id1), Identifier(id2), Wire(w))

      case NotConstantRegex(v, w) =>
        Not(Constant(v.toInt), Wire(w))

      case NotIdRegex(id, w) =>
        Not(Identifier(id), Wire(w))

      case LeftShiftConstantRegex(v, n, w) =>
        LeftShift(Constant(v.toInt), n.toInt, Wire(w))

      case LeftShiftIdRegex(id, n, w) =>
        LeftShift(Identifier(id), n.toInt, Wire(w))

      case RightShiftConstantRegex(v, n, w) =>
        RightShift(Constant(v.toInt), n.toInt, Wire(w))

      case RightShiftIdRegex(id, n, w) =>
        RightShift(Identifier(id), n.toInt, Wire(w))

      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }
  }

  private def part1(instructions: Seq[Instruction]): Unit = {
    val answer = 0
    println(s"part 1 answer: $answer")
  }
}
