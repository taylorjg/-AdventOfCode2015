import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    // val lines = Source.fromResource("test.txt").getLines().toList
    val pairings = parseLines(lines)
    val part1Answer = part1(pairings)
    println(s"part 1 answer: $part1Answer")
  }

  final case class Line(person1: String, person2: String, delta: Int)

  final case class Pairing(person1: String,
                           person2: String,
                           gain: Int,
                           loss: Int)

  private final val LineRegex =
    raw"""(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.""".r

  private def parseLines(strings: Seq[String]): Seq[Pairing] = {
    val lines = strings.map(parseLine)
    val grouped = lines.groupBy { line =>
      val names = List(line.person1, line.person2).sorted
      s"${names.head}:${names.last}"
    }
    grouped.values
      .map(v =>
        Pairing(v.head.person1, v.head.person2, v.head.delta, v.last.delta))
      .toSeq
  }

  private def parseLine(line: String): Line =
    line match {
      case LineRegex(p1, "gain", dh, p2) => Line(p1, p2, +dh.toInt)
      case LineRegex(p1, "lose", dh, p2) => Line(p1, p2, -dh.toInt)
      case _                             => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def totalHappinessChange(arrangement: Seq[Pairing]): Int =
    arrangement.map(p => p.gain + p.loss).sum

  private def distinctPeople(pairings: Seq[Pairing]): Int =
    pairings.flatMap(p => List(p.person1, p.person2)).distinct.length

  private def allPeopleMentionedTwice(pairings: Seq[Pairing]): Boolean = {
    val allPersons = pairings.flatMap(p => List(p.person1, p.person2))
    val distinctPersons = allPersons.distinct
    distinctPersons.forall(p => allPersons.count(_ == p) == 2)
  }

  private def part1(pairings: Seq[Pairing]): Int = {
    val numPeople = distinctPeople(pairings)
    pairings
      .combinations(numPeople)
      .filter(allPeopleMentionedTwice)
      .map(totalHappinessChange)
      .max
  }
}
