import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").mkString
    part1(input)
    part2(input)
  }

  private def part1(input: String): Unit = {
    val (openParens, closeParens) = input.partition(_ == '(')
    println(s"part 1 answer: ${openParens.length - closeParens.length}")
  }

  private def part2(input: String): Unit = {

    @tailrec
    def loop(floor: Int, steps: Int): Option[Int] = {
      if (!input.isDefinedAt(steps)) None else {
        val delta = if (input(steps) == '(') +1 else -1
        val newFloor = floor + delta
        val newSteps = steps + 1
        if (newFloor < 0) Some(newSteps)
        else loop(newFloor, newSteps)
      }
    }

    val maybeSteps = loop(0, 0)
    println(s"part 2 answer: $maybeSteps")
  }
}
