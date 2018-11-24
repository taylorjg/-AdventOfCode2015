import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    // val lines = Source.fromResource("test.txt").getLines().toList
    val pairings = parseLines(lines)
    val part1Answer = optimalArrangement(pairings)
    println(s"part 1 answer: $part1Answer")
    val pairingsWithMe = addMe(pairings)
    val part2Answer = optimalArrangement2(pairingsWithMe)
    println(s"part 2 answer: $part2Answer")
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

  // part 1 answer: 733
  // Pairing(David,George,2,92)
  // Pairing(David,Eric,-18,51)
  // Pairing(Alice,Bob,2,40)
  // Pairing(Alice,Mallory,39,92)
  // Pairing(Carol,Eric,95,100)
  // Pairing(George,Mallory,97,-89)
  // Pairing(Bob,Frank,41,91)
  // Pairing(Carol,Frank,90,8)

  // part 2 answer: 755
  // Pairing(David,George,2,92)
  // Pairing(Alice,Bob,2,40)
  // Pairing(Carol,Mallory,94,-51)
  // Pairing(Eric,Frank,21,97)
  // Pairing(Alice,Mallory,39,92)
  // Pairing(Carol,Eric,95,100)
  // Pairing(Bob,Frank,41,91)
  // Pairing(** ME **,David,0,0)
  // Pairing(** ME **,George,0,0)

  type Person = String
  type Coords = (Person, Person)
  type Matrix = List[Coords]

  private def buildAdjacencyMatrix(pairings: Seq[Pairing]): Matrix =
    pairings.foldLeft(List[Coords]()) { (acc, p) =>
      (p.person1, p.person2) :: (p.person2, p.person1) :: acc
    }

  private def checkAdjacencyMatrix(pairings: Seq[Pairing]): Boolean = {
    val people = pairings.flatMap(p => List(p.person1, p.person2)).distinct
    val matrix = buildAdjacencyMatrix(pairings)
    val rows = people.map(p => matrix.filter(_._1 == p))
    val cols = people.map(p => matrix.filter(_._2 == p))
    rows.forall(_.length == 2) && cols.forall(_.length == 2)
  }

  private def optimalArrangement(pairings: Seq[Pairing]): Int = {
    val numPeople = distinctPeople(pairings)
    val winner = pairings
      .combinations(numPeople)
      .filter(checkAdjacencyMatrix)
      .maxBy(totalHappinessChange)
    println(s"winner: $winner")
    totalHappinessChange(winner)
  }

  private def optimalArrangement2(pairings: Seq[Pairing]): Int = {
    val numPeople = distinctPeople(pairings)
    val winner = pairings
      .combinations(numPeople)
      .filter(includesMe)
      .filter(checkAdjacencyMatrix)
      .maxBy(totalHappinessChange)
    println(s"winner: $winner")
    totalHappinessChange(winner)
  }

  private def addMe(pairings: Seq[Pairing]): Seq[Pairing] = {
    val distinctPersons =
      pairings.flatMap(p => List(p.person1, p.person2)).distinct
    val additions = distinctPersons.map(p => Pairing("** ME **", p, 0, 0))
    pairings ++ additions
  }

  private def includesMe(pairings: Seq[Pairing]): Boolean = {
    val allPersons = pairings.flatMap(p => List(p.person1, p.person2))
    allPersons.contains("** ME **")
  }
}
