import scalaz._
import Scalaz._
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("input.txt").getLines().toList
    val instructions = parseLines(lines)

    val part1Answer = run(instructions)
    println(s"part 1 answer: $part1Answer")

    val part2Answer = run(instructions, Wire("b") -> part1Answer)
    println(s"part 2 answer: $part2Answer")
  }

  type Signal = Long

  sealed abstract class Source
  final case class Value(signal: Signal) extends Source
  final case class Wire(name: String) extends Source

  type Wires = Map[Wire, Signal]

  sealed abstract class Instruction
  final case class Constant(input: Value, output: Wire) extends Instruction
  final case class Passthrough(input: Wire, output: Wire) extends Instruction
  final case class Not(input: Source, output: Wire) extends Instruction
  final case class LeftShift(by: Int, input: Source, output: Wire) extends Instruction
  final case class RightShift(by: Int, input: Source, output: Wire) extends Instruction
  final case class And(input1: Source, input2: Wire, output: Wire) extends Instruction
  final case class Or(input1: Source, input2: Source, output: Wire) extends Instruction

  private final val NumberRegex = """(\d+)"""
  private final val WireRegex = """([a-z]+)"""
  private final val SourceRegex = """(\d+|[a-z]+)"""
  private final val ConstantRegex = s"$NumberRegex -> $WireRegex".r
  private final val PassthroughRegex = s"$WireRegex -> $WireRegex".r
  private final val NotRegex = s"NOT $SourceRegex -> $WireRegex".r
  private final val LeftShiftRegex = s"$SourceRegex LSHIFT $NumberRegex -> $WireRegex" r
  private final val RightShiftRegex = s"$SourceRegex RSHIFT $NumberRegex -> $WireRegex".r
  private final val AndRegex = s"$SourceRegex AND $SourceRegex -> $WireRegex".r
  private final val OrRegex = s"$SourceRegex OR $SourceRegex -> $WireRegex".r

  private def parseLines(lines: Seq[String]): Seq[Instruction] =
    lines.map(parseLine)

  private def parseLine(line: String): Instruction =
    line match {

      case ConstantRegex(s, w) =>
        Constant(Value(s.toLong), Wire(w))

      case PassthroughRegex(s, w) =>
        Passthrough(Wire(s), Wire(w))

      case NotRegex(s, w) =>
        Not(Wire(s), Wire(w))

      case LeftShiftRegex(s, by, w) =>
        LeftShift(by.toInt, Wire(s), Wire(w))

      case RightShiftRegex(s, by, w) =>
        RightShift(by.toInt, Wire(s), Wire(w))

      case AndRegex(s1, s2, w) =>
        And(stringToSource(s1), Wire(s2), Wire(w))

      case OrRegex(s1, s2, w) =>
        Or(Wire(s1), Wire(s2), Wire(w))

      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def stringToSource(s: String): Source =
    if (s.forall(Character.isDigit)) Value(s.toLong) else Wire(s)

  private def setOutput(wires: Wires,
                        output: Wire,
                        value: => Option[Signal]): Wires =
    (wires.isDefinedAt(output), value) match {
      case (false, Some(signal)) => wires + (output -> signal)
      case _                     => wires
    }

  private def tryGetValue(wires: Wires, input: Source): Option[Signal] =
    input match {
      case Value(signal) => Some(signal)
      case wire: Wire    => wires.get(wire)
    }

  private def zeroInputGate(wires: Wires, value: Value, output: Wire): Wires =
    setOutput(wires, output, value.signal.some)

  private def oneInputGate(wires: Wires,
                           input: Source,
                           output: Wire,
                           eval: Signal => Signal): Wires = {
    val value = tryGetValue(wires, input).map(eval)
    setOutput(wires, output, value)
  }

  private def twoInputGate(wires: Wires,
                           input1: Source,
                           input2: Source,
                           output: Wire,
                           eval: (Signal, Signal) => Signal): Wires = {
    val value = (tryGetValue(wires, input1) |@| tryGetValue(wires, input2))(eval)
    setOutput(wires, output, value)
  }

  private def processInstruction(wires: Wires,
                                 instruction: Instruction): Wires =
    instruction match {
      case Constant(input, output) =>
        zeroInputGate(wires, input, output)
      case Passthrough(input, output) =>
        oneInputGate(wires, input, output, identity)
      case Not(input, output) =>
        oneInputGate(wires, input, output, v => ~v & 0xFFFF)
      case LeftShift(by, input, output) =>
        oneInputGate(wires, input, output, v => v << by)
      case RightShift(by, input, output) =>
        oneInputGate(wires, input, output, v => v >> by)
      case And(input1, input2, output) =>
        twoInputGate(wires, input1, input2, output, (v1, v2) => v1 & v2)
      case Or(input1, input2, output) =>
        twoInputGate(wires, input1, input2, output, (v1, v2) => v1 | v2)
    }

  private def processInstructions(wires: Wires,
                                  instructions: Seq[Instruction]): Wires =
    instructions.foldLeft(wires)(processInstruction)

  private def run(instructions: Seq[Instruction],
                  overrides: (Wire, Signal)*): Signal = {
    def loop(wires: Wires): Signal =
      wires.getOrElse(Wire("a"), {
        loop(processInstructions(wires, instructions))
      })
    val initialWires: Wires = Map(overrides: _*)
    loop(initialWires)
  }
}
