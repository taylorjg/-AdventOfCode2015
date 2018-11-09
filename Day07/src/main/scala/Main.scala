import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val instructions = parseLines(lines)

    part1(instructions)

    val instructions2 = instructions.map {
      case RightShiftInstruction(1, Wire("b"), Wire("v")) =>
        RightShiftInstruction(1, Value(3176), Wire("v"))
      case otherwise => otherwise
    }
    part1(instructions2)
  }

  type Signal = Long

  sealed abstract class Source
  final case class Wire(name: String) extends Source
  final case class Value(value: Signal) extends Source

  sealed trait Gate {
    val getOutputValue: Future[Signal]
  }
  final case class Constant(input: Future[GetOutputValue]) extends Gate {
    override val getOutputValue: Future[Signal] = for {
      f <- input
      v <- f
    } yield v
  }
  final case class And(input1: Future[GetOutputValue], input2: Future[GetOutputValue]) extends Gate {
    override val getOutputValue: Future[Signal] = for {
      f1 <- input1
      f2 <- input2
      v1 <- f1
      v2 <- f2
    } yield v1 & v2
  }
  final case class Or(input1: Future[GetOutputValue], input2: Future[GetOutputValue]) extends Gate {
    override val getOutputValue: Future[Signal] = for {
      f1 <- input1
      f2 <- input2
      v1 <- f1
      v2 <- f2
    } yield v1 | v2
  }
  final case class Not(input: Future[GetOutputValue]) extends Gate {
    override val getOutputValue: Future[Signal] = for {
      f <- input
      v <- f
    } yield ~v & 0xFFFF
  }
  final case class LeftShift(by: Int, input: Future[GetOutputValue]) extends Gate {
    override val getOutputValue: Future[Signal] = for {
      f <- input
      v <- f
    } yield v << by
  }
  final case class RightShift(by: Int, input: Future[GetOutputValue]) extends Gate {
    override val getOutputValue: Future[Signal] = for {
      f <- input
      v <- f
    } yield v >> by
  }

  sealed abstract class Instruction
  final case class ConstantInstruction(input: Source, output: Wire) extends Instruction
  final case class AndInstruction(input1: Source, input2: Wire, output: Wire) extends Instruction
  final case class OrInstruction(input1: Source, input2: Source, output: Wire) extends Instruction
  final case class NotInstruction(input: Source, output: Wire) extends Instruction
  final case class LeftShiftInstruction(by: Int, input: Source, output: Wire) extends Instruction
  final case class RightShiftInstruction(by: Int, input: Source, output: Wire) extends Instruction

  private def stringToSource(s: String): Source =
    if (s.forall(Character.isDigit)) Value(s.toLong) else Wire(s)

  private def parseLines(lines: Seq[String]): Seq[Instruction] =
    lines.map(parseLine)

  private final val ConstantRegex = """^(\d+|[a-z]+) -> ([a-z]+)$""".r
  private final val AndRegex = """^(\d+|[a-z]+) AND (\d+|[a-z]+) -> ([a-z]+)$""".r
  private final val OrRegex = """^(\d+|[a-z]+) OR (\d+|[a-z]+) -> ([a-z]+)$""".r
  private final val NotRegex = """^NOT (\d+|[a-z]+) -> ([a-z]+)$""".r
  private final val LeftShiftRegex = """^(\d+|[a-z]+) LSHIFT (\d+) -> ([a-z]+)$""".r
  private final val RightShiftRegex = """^(\d+|[a-z]+) RSHIFT (\d+) -> ([a-z]+)$""".r

  private def parseLine(line: String): Instruction = {
    line match {

      case ConstantRegex(s, w) =>
        ConstantInstruction(stringToSource(s), Wire(w))

      case AndRegex(s1, s2, w) =>
        AndInstruction(stringToSource(s1), Wire(s2), Wire(w))

      case OrRegex(s1, s2, w) =>
        OrInstruction(Wire(s1), Wire(s2), Wire(w))

      case NotRegex(s, w) =>
        NotInstruction(Wire(s), Wire(w))

      case LeftShiftRegex(s, by, w) =>
        LeftShiftInstruction(by.toInt, Wire(s), Wire(w))

      case RightShiftRegex(s, by, w) =>
        RightShiftInstruction(by.toInt, Wire(s), Wire(w))

      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }
  }

  type GetOutputValue = Future[Signal]
  type Wires = Map[String, Promise[GetOutputValue]]

  private def part1(instructions: Seq[Instruction]): Unit = {

    def op(wires: Wires, instruction: Instruction): Wires = {

      def lookupSource(wires: Wires, source: Source): (Wires, Future[GetOutputValue]) =
        source match {
          case Wire(name) =>
            wires.get(name) match {
              case Some(p) =>
                (wires, p.future)
              case None =>
                val p = Promise[GetOutputValue]()
                val wires2 = wires.updated(name, p)
                (wires2, p.future)
            }
          case Value(value) =>
            val p = Promise[GetOutputValue].success(Future.successful(value))
            (wires, p.future)
        }

      def lookupOutput(wires: Wires, wire: Wire): (Wires, Promise[GetOutputValue]) = {
        wires.get(wire.name) match {
          case Some(p) =>
            (wires, p)
          case None =>
            val p = Promise[GetOutputValue]()
            val wires2 = wires.updated(wire.name, p)
            (wires2, p)
        }
      }

      def connect(wires: Wires, output: Wire, gate: Gate): Wires = {
        val (wires2, promise) = lookupOutput(wires, output)
        promise.success(gate.getOutputValue)
        wires2
      }

      instruction match {

        case ConstantInstruction(input, output) =>
          val (wires2, inputF) = lookupSource(wires, input)
          connect(wires2, output, Constant(inputF))

        case AndInstruction(input1, input2, output) =>
          val (wires2, input1F) = lookupSource(wires, input1)
          val (wires3, input2F) = lookupSource(wires2, input2)
          connect(wires3, output, And(input1F, input2F))

        case OrInstruction(input1, input2, output) =>
          val (wires2, input1F) = lookupSource(wires, input1)
          val (wires3, input2F) = lookupSource(wires2, input2)
          connect(wires3, output, Or(input1F, input2F))

        case NotInstruction(input, output) =>
          val (wires2, inputF) = lookupSource(wires, input)
          connect(wires2, output, Not(inputF))

        case LeftShiftInstruction(by, input, output) =>
          val (wires2, inputF) = lookupSource(wires, input)
          connect(wires2, output, LeftShift(by, inputF))

        case RightShiftInstruction(by, input, output) =>
          val (wires2, inputF) = lookupSource(wires, input)
          connect(wires2, output, RightShift(by, inputF))
      }
    }

    val initialWires: Wires = Map()
    val finalWires = instructions.foldLeft(initialWires)(op)

    finalWires.get("a").map(_.future).map(_.map(_.map(a => println(s"a: $a"))))
  }
}
