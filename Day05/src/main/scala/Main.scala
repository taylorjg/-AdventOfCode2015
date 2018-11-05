import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines.toSeq
    part1(lines)
  }

  private def part1(lines: Seq[String]): Unit = {
    val answer = lines.count(checkString)
    println(s"part 1 answer: $answer")
  }

  private final val Vowels = List('a', 'e', 'i', 'o', 'u')

  private def hasMinThreeVowels(s: String): Boolean =
    s.filter(Vowels.contains(_)).length >= 3

  private def hasDoubleLetter(s: String): Boolean =
    s.sliding(2).exists(pr => pr(0) == pr(1))

  private final val DisallowedValues = List(
    "ab",
    "cd",
    "pq",
    "xy"
  )

  private def hasNoDisallowedValues(s: String): Boolean =
    DisallowedValues.map(s.contains(_)).forall(b => !b)

  private def checkString(s: String): Boolean = {
    hasMinThreeVowels(s) &&
    hasDoubleLetter(s) &&
    hasNoDisallowedValues(s)
  }
}
