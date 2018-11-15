import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val literals = Source.fromResource("input.txt").getLines().toList
    val part1Answer = part1(literals)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(literals)
    println(s"part 2 answer: $part2Answer")
  }

  private final val DoubleBackslash = "\\\\"
  private final val BackslashQuote = "\\\""
  private final val HexRegex =  """(\\x[\da-f]{2})""".r

  private def decode(s: String): String = {
    @tailrec
    def loop(remaining: String, acc: String): String = {
      val maybeMatch = HexRegex.findFirstMatchIn(remaining)
      val pos1 = remaining.indexOf(DoubleBackslash)
      val pos2 = remaining.indexOf(BackslashQuote)
      val pos3 = maybeMatch.map(m => m.start).getOrElse(-1)
      val poss = List(pos1, pos2, pos3)
      if (poss.forall(_ < 0)) {
        acc + remaining
      } else {
        val minPos = poss.filter(_ >= 0).min
        minPos match {
          case p if p == pos1 =>
            loop(remaining.substring(p + DoubleBackslash.length), acc + remaining.take(p) + "\\")
          case p if p == pos2 =>
            loop(remaining.substring(p + BackslashQuote.length), acc + remaining.take(p) + "\"")
          case p if p == pos3 =>
            maybeMatch match {
              case Some(m) =>
                val code = (m.matched(2) - '0') * 16 + (m.matched(3) - '0')
                val ch = code.toChar
                loop(remaining.substring(m.end), acc + remaining.take(m.start) + ch)
              case None =>
                acc + remaining
            }
        }
      }
    }
    val unquoted = s.drop(1).init
    loop(unquoted, "")
  }

  private def encode(s: String): String = {
    s
  }

  private def part1(literals: List[String]): Int = {
    def f(s: String): (Int, Int) = {
      val len1 = s.length
      val s2 = decode(s)
      val len2 = s2.length
      println(s"s: $s, s2: $s2; len1: $len1; len2: $len2")
      (len1, len2)
    }
    val lenPairs = literals.map(f)
    val (sum1, sum2) = lenPairs.foldLeft((0, 0)) {
      case ((acc1, acc2), (len1, len2)) => (acc1 + len1, acc2 + len2)
    }
    sum1 - sum2
  }

  private def part2(literals: List[String]): Int = {
    def f(s: String): (Int, Int) = {
      val len1 = s.length
      val s2 = encode(s)
      val len2 = s2.length
      println(s"s: $s, s2: $s2; len1: $len1; len2: $len2")
      (len1, len2)
    }
    val lenPairs = literals.map(f)
    val (sum1, sum2) = lenPairs.foldLeft((0, 0)) {
      case ((acc1, acc2), (len1, len2)) => (acc1 + len1, acc2 + len2)
    }
    sum2 - sum1
  }
}
