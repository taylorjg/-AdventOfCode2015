import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    val input = "1321131112"
    val part1Answer = part1(input)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(input)
    println(s"part 2 answer: $part2Answer")
  }

  private def part1(input: String): Int =
    Iterable.iterate(input, 41)(generateNextString).last.length

  private def part2(input: String): Int =
    Iterable.iterate(input, 51)(generateNextString).last.length

  private def generateNextString(input: String): String = {
    val sb = StringBuilder.newBuilder
    @tailrec
    def loop(currentDigitIndex: Int = 0): Unit = {
      if (input.isDefinedAt(currentDigitIndex)) {
        val currentDigit = input(currentDigitIndex)
        @tailrec
        def findEndOfRun(nextDigitIndex: Int): Int =
          if (input.isDefinedAt(nextDigitIndex) && input(nextDigitIndex) == currentDigit)
            findEndOfRun(nextDigitIndex + 1)
          else nextDigitIndex
        val runLength = findEndOfRun(currentDigitIndex + 1) - currentDigitIndex
        sb ++= s"$runLength$currentDigit"
        loop(currentDigitIndex + runLength)
      }
    }
    loop()
    sb.toString
  }
}
