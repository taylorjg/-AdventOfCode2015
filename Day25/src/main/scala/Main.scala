import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    val (row, col) = (2981, 3075)
    val part1Answer = part1(row, col)
    println(s"part 1 answer: $part1Answer")
  }

  type Code = Long

  private final val FirstCode: Code = 20151125
  private final val Multiplier: Code = 252533
  private final val Denominator: Code = 33554393

  private def part1(row: Int, col: Int): Code = {
    val number = rowColToNumber(row, col)
    println(s"number: $number")
    nthCode(number)
  }

  private def nthCode(n: Int): Code = {
    @tailrec
    def loop(remaining: Int, code: Code): Code = {
      if (remaining == 1) code
      else loop(remaining - 1, nextCode(code))
    }
    loop(n, FirstCode)
  }

  private def nextCode(code: Code): Code =
    code * Multiplier % Denominator

  private def rowColToNumber(row: Int, col: Int): Int = {
    val v1 = sumOfNumbers(col)
    val v2 = (row - 1) * col + sumOfNumbers(row - 2)
    v1 + v2
  }

  private def sumOfNumbers(n: Int): Int =
    ((n + 1) * (n.toDouble / 2)).toInt
}
