import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
//import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

//    val lines = Source.fromResource("input.txt").getLines().toList
//    val instructions = parseLines(lines)

    val instructions = Seq(
      NotInstruction(Wire("x"), Wire("h")),
      NotInstruction(Wire("y"), Wire("i")),
      LeftShiftInstruction(2, Wire("x"), Wire("f")),
      RightShiftInstruction(2, Wire("y"), Wire("g")),
      AndInstruction(Wire("x"), Wire("y"), Wire("d")),
      OrInstruction(Wire("x"), Wire("y"), Wire("e")),
      ConstantInstruction(Value(123), Wire("x")),
      ConstantInstruction(Value(456), Wire("y")),
      PassthroughInstruction(Wire("f"), Wire("p"))
    )

    println(instructions)
    part1(instructions)
  }

  type Signal = Long

  sealed abstract class Source
  final case class Wire(name: String) extends Source
  final case class Value(value: Signal) extends Source

  sealed trait Gate {
    val getOutputValue: Future[Signal]
  }
  final case class Constant(value: Value) extends Gate {
    override val getOutputValue: Future[Signal] = Future.successful(value.value)
  }
  final case class Passthrough(input: Future[GetOutputValue]) extends Gate {
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
  final case class ConstantInstruction(value: Value, output: Wire) extends Instruction
  final case class PassthroughInstruction(input: Wire, output: Wire) extends Instruction
  final case class AndInstruction(input1: Wire, input2: Wire, output: Wire) extends Instruction
  final case class OrInstruction(input1: Wire, input2: Wire, output: Wire) extends Instruction
  final case class NotInstruction(input: Wire, output: Wire) extends Instruction
  final case class LeftShiftInstruction(by: Int, input: Wire, output: Wire) extends Instruction
  final case class RightShiftInstruction(by: Int, input: Wire, output: Wire) extends Instruction

//  private def stringToSource(s: String): Source =
//    if (s.forall(Character.isDigit)) Value(s.toLong) else Wire(s)

//  private def parseLines(lines: Seq[String]): Seq[Instruction] =
//    lines.map(parseLine)

//  private final val ConstantRegex = """^(\d+) -> ([a-z]+)$""".r
//  private final val PassthroughRegex = """^([a-z]+) -> ([a-z]+)$""".r
//  private final val AndRegex = """^(\d+|[a-z]+) AND (\d+|[a-z]+) -> ([a-z]+)$""".r
//  private final val OrRegex = """^(\d+|[a-z]+) OR (\d+|[a-z]+) -> ([a-z]+)$""".r
//  private final val NotRegex = """^NOT (\d+|[a-z]+) -> ([a-z]+)$""".r
//  private final val LeftShiftRegex = """^(\d+|[a-z]+) LSHIFT (\d+) -> ([a-z]+)$""".r
//  private final val RightShiftRegex = """^(\d+|[a-z]+) RSHIFT (\d+) -> ([a-z]+)$""".r

//  private def parseLine(line: String): Instruction = {
//    line match {
//
//      case ConstantRegex(s, w) =>
//        ConstantInstruction(Value(s.toLong), Wire(w))
//
//      case PassthroughRegex(s, w) =>
//        PassthroughInstruction(Wire(s), Wire(w))
//
//      case AndRegex(s1, s2, w) =>
//        AndInstruction(Wire(s1), Wire(s2), Wire(w))
//
//      case OrRegex(s1, s2, w) =>
//        OrInstruction(Wire(s1), Wire(s2), Wire(w))
//
//      case NotRegex(s, w) =>
//        NotInstruction(Wire(s), Wire(w))
//
//      case LeftShiftRegex(s, by, w) =>
//        LeftShiftInstruction(by.toInt, Wire(s), Wire(w))
//
//      case RightShiftRegex(s, by, w) =>
//        RightShiftInstruction(by.toInt, Wire(s), Wire(w))
//
//      case _ => throw new Exception(s"Failed to parse line, '$line'.")
//    }
//  }

  type GetOutputValue = Future[Signal]
  type Wires = Map[String, Promise[GetOutputValue]]

  private def part1(instructions: Seq[Instruction]): Unit = {

    def op(wires: Wires, instruction: Instruction): Wires = {
      def lookupWire(wires: Wires, wire: Wire): (Wires, Promise[GetOutputValue]) = {
        wires.get(wire.name) match {
          case Some(p) =>
            (wires, p)
          case None =>
            val p = Promise[GetOutputValue]()
            val newWires = wires.updated(wire.name, p)
            (newWires, p)
        }
      }

      def connect(wires: Wires, output: Wire, gate: Gate): Wires = {
        val (newWires, promise) = lookupWire(wires, output)
        promise.success(gate.getOutputValue)
        newWires
      }

      instruction match {

        case ConstantInstruction(value, output) =>
          connect(wires, output, Constant(value))

        case PassthroughInstruction(input, output) =>
          val (wires2, b) = lookupWire(wires, input)
          connect(wires2, output, Passthrough(b.future))

        case AndInstruction(input1, input2, output) =>
          val (wires2, b) = lookupWire(wires, input1)
          val (wires3, d) = lookupWire(wires2, input2)
          connect(wires3, output, And(b.future, d.future))

        case OrInstruction(input1, input2, output) =>
          val (wires2, b) = lookupWire(wires, input1)
          val (wires3, d) = lookupWire(wires2, input2)
          connect(wires3, output, Or(b.future, d.future))

        case NotInstruction(input, output) =>
          val (wires2, b) = lookupWire(wires, input)
          connect(wires2, output, Not(b.future))

        case LeftShiftInstruction(by, input, output) =>
          val (wires2, b) = lookupWire(wires, input)
          connect(wires2, output, LeftShift(by, b.future))

        case RightShiftInstruction(by, input, output) =>
          val (wires2, b) = lookupWire(wires, input)
          connect(wires2, output, RightShift(by, b.future))
      }
    }

    val initialWires: Wires = Map()
    val finalWires = instructions.foldLeft(initialWires)(op)
    val kvps = finalWires.toList
    kvps.foreach {
      kvp =>
        val name = kvp._1
        val future = kvp._2.future
        future.foreach {
          fn =>
            fn.foreach {
              signal =>
                println(s"$name: $signal")
            }
        }
    }
  }
}
