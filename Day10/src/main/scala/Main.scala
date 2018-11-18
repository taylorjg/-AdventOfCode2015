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
    Stream.iterate(input, 41)(generateNextString).last.length

  private def part2(input: String): Int =
    Stream.iterate(input, 51)(generateNextString).last.length

  private def generateNextString(s: String): String = {
    @tailrec
    def loop(remaining: String, acc: List[String]): List[String] = {
      if (remaining.isEmpty) acc
      else {
        val ch = remaining.head
        val (run, remaining2) = remaining.span(_ == ch)
        val say = s"${run.length}$ch"
        loop(remaining2, say :: acc)
      }
    }
    val says = loop(s, List()).reverse
    val sb = new StringBuilder
    says.foldLeft(sb)((sb, s) => sb ++= s)
    sb.toString
  }
}
