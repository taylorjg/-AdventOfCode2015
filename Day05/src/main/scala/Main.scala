import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines.toSeq
    part1(lines)
    part2(lines)
  }

  private def part1(lines: Seq[String]): Unit = {
    val answer = lines.count(checkString1)
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

  private def checkString1(s: String): Boolean =
    hasMinThreeVowels(s) &&
    hasDoubleLetter(s) &&
    hasNoDisallowedValues(s)

  private def part2(lines: Seq[String]): Unit = {
    val answer = lines.count(checkString2)
    println(s"part 1 answer: $answer")
  }

  private def newRule1(s: String): Boolean = {
    val prs = s.sliding(2).zipWithIndex
    val bs = prs.map {
      case (pr, index) => s.indexOf(pr, index + 2) > 0
    }
    bs.exists(identity)
  }

  private def newRule2(s: String): Boolean =
    s.sliding(3).exists(pr => pr(0) == pr(2))

  private def checkString2(s: String): Boolean =
    newRule1(s) && newRule2(s)
}
