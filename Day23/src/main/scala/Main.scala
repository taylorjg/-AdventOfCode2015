import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toVector
    val instructions = parseLines(lines)
    val part1Answer = part1(instructions)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(instructions)
    println(s"part 2 answer: $part2Answer")
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

  type Registers = Map[String, Int]
  final case class State(registers: Registers, ip: Int)

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

  private def part1(instructions: Vector[Instruction]): Int =
    runInstructions(instructions, Map())("b")

  private def part2(instructions: Vector[Instruction]): Int =
    runInstructions(instructions, Map("a" -> 1))("b")

  private def runInstructions(instructions: Vector[Instruction],
                              initialRegisters: Registers): Registers = {
    val initialState = State(initialRegisters, 0)
    @tailrec
    def loop(state: State): State =
      if (instructions.isDefinedAt(state.ip))
        loop(processInstruction(state, instructions(state.ip)))
      else
        state
    val finalState = loop(initialState)
    finalState.registers
  }

  private def processInstruction(state: State,
                                 instruction: Instruction): State = {
    val rs = state.registers
    instruction match {
      case Hlf(r) =>
        val v = rs.getOrElse(r, 0)
        state.copy(rs.updated(r, v >> 1), ip = state.ip + 1)
      case Tpl(r) =>
        val v = rs.getOrElse(r, 0)
        state.copy(rs.updated(r, v * 3), ip = state.ip + 1)
      case Inc(r) =>
        val v = rs.getOrElse(r, 0)
        state.copy(rs.updated(r, v + 1), ip = state.ip + 1)
      case Jmp(offset) =>
        state.copy(ip = state.ip + offset)
      case Jie(r, offset) =>
        val v = rs.getOrElse(r, 0)
        val offset2 = if (isEven(v)) offset else 1
        state.copy(ip = state.ip + offset2)
      case Jio(r, offset) =>
        val v = rs.getOrElse(r, 0)
        val offset2 = if (isOne(v)) offset else 1
        state.copy(ip = state.ip + offset2)
    }
  }

  private def isEven(n: Int): Boolean =
    n % 2 == 0

  private def isOne(n: Int): Boolean =
    n == 1
}
