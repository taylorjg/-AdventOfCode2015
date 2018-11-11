import scalaz._
import Scalaz._
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val gates = parseLines(lines)
    val part1Answer = runCircuit(gates)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = runCircuit(gates, Wire("b") -> part1Answer)
    println(s"part 2 answer: $part2Answer")
  }

  sealed abstract class Source
  final case class Value(signal: Signal) extends Source
  final case class Wire(name: String) extends Source

  type Signal = Long
  type Wires = Map[Wire, Signal]
  type Gate = Wires => Wires

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

  private def parseLines(lines: Seq[String]): Seq[Gate] =
    lines.map(parseLine)

  private def parseLine(line: String): Gate =
    line match {
      case ConstantRegex(s, w) =>
        zeroInputGate(Value(s.toLong), Wire(w))
      case PassthroughRegex(s, w) =>
        oneInputGate(Wire(s), Wire(w), identity)
      case NotRegex(s, w) =>
        oneInputGate(toSource(s), Wire(w), v => ~v & 0xFFFF)
      case LeftShiftRegex(s, by, w) =>
        oneInputGate(toSource(s), Wire(w), v => v << by.toInt)
      case RightShiftRegex(s, by, w) =>
        oneInputGate(toSource(s), Wire(w), v => v >> by.toInt)
      case AndRegex(s1, s2, w) =>
        twoInputGate(toSource(s1), toSource(s2), Wire(w), (v1, v2) => v1 & v2)
      case OrRegex(s1, s2, w) =>
        twoInputGate(toSource(s1), toSource(s2), Wire(w), (v1, v2) => v1 | v2)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def toSource(s: String): Source =
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
      case value: Value => value.signal.some
      case wire: Wire   => wires.get(wire)
    }

  private def zeroInputGate(value: Value, output: Wire)(wires: Wires): Wires =
    setOutput(wires, output, value.signal.some)

  private def oneInputGate(input: Source, output: Wire, eval: Signal => Signal)(
      wires: Wires): Wires =
    setOutput(wires, output, tryGetValue(wires, input).map(eval))

  private def twoInputGate(
      input1: Source,
      input2: Source,
      output: Wire,
      eval: (Signal, Signal) => Signal)(wires: Wires): Wires =
    setOutput(wires,
              output,
              (tryGetValue(wires, input1) |@| tryGetValue(wires, input2))(eval))

  private def evalGates(initialWires: Wires, gates: Seq[Gate]): Wires =
    gates.foldLeft(initialWires)((wires, gate) => gate(wires))

  private def runCircuit(gates: Seq[Gate], overrides: (Wire, Signal)*): Signal = {
    def loop(wires: Wires): Signal = wires.getOrElse(Wire("a"), loop(evalGates(wires, gates)))
    val initialWires: Wires = Map(overrides: _*)
    loop(initialWires)
  }
}
