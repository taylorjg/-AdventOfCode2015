import scala.annotation.tailrec
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

  type Person = String
  type Coords = (Person, Person)
  type Matrix = List[Coords]

  private def buildAdjacencyMatrix(pairings: Seq[Pairing]): Matrix =
    pairings.foldLeft(List[Coords]()) { (acc, p) =>
      (p.person1, p.person2) :: (p.person2, p.person1) :: acc
    }

  // TODO: http://vixra.org/pdf/1210.0049v1.pdf
  // "In an adjacency matrix which encodes for a directed Hamiltonian path, a non-zero determinant
  // value certifies the existence of a directed Hamiltonian path when no zero rows (columns) and
  // no similar rows (columns) exist in the adjacency matrix."
  // https://www-cs-faculty.stanford.edu/~knuth/fasc8a.ps.gz
  private def checkAdjacencyMatrix(pairings: Seq[Pairing]): Boolean = {
    val people = pairings.flatMap(p => List(p.person1, p.person2)).distinct
    val matrix = buildAdjacencyMatrix(pairings)
    val rows = people.map(p => matrix.filter(_._1 == p))
    val cols = people.map(p => matrix.filter(_._2 == p))
    rows.forall(_.length == 2) && cols.forall(_.length == 2)
  }

  private def optimalArrangement(pairings: Seq[Pairing]): Int = {
    val numPeople = distinctPeople(pairings)
    pairings
      .combinations(numPeople)
      .filter(checkAdjacencyMatrix)
      .map(totalHappinessChange)
      .max
  }

  private def optimalArrangement2(pairings: Seq[Pairing]): Int = {
    val numPeople = distinctPeople(pairings)
    pairings
      .combinations(numPeople)
      .filter(checkAdjacencyMatrix)
      .filter(includesMe)
      .filter(everybodyCanReachMe)
      .map(totalHappinessChange)
      .max
  }

  private final val Me = "** ME **"

  private def addMe(pairings: Seq[Pairing]): Seq[Pairing] = {
    val distinctPersons =
      pairings.flatMap(p => List(p.person1, p.person2)).distinct
    val additions = distinctPersons.map(p => Pairing(Me, p, 0, 0))
    pairings ++ additions
  }

  private def includesMe(pairings: Seq[Pairing]): Boolean =
    pairings.flatMap(p => List(p.person1, p.person2)).contains(Me)

  private def everybodyCanReachMe(pairings: Seq[Pairing]): Boolean = {
    val people = pairings.flatMap(p => List(p.person1, p.person2)).distinct
    people.filterNot(_ == Me).forall(canReachMe(pairings))
  }

  private def canReachMe(pairings: Seq[Pairing])(person: String): Boolean = {

    def findPerson(pairings: Seq[Pairing], person: String): Option[Pairing] =
      pairings.find(pairing =>
        pairing.person1 == person || pairing.person2 == person)

    @tailrec
    def loop(pairings: Seq[Pairing], person: String): Boolean = {
      findPerson(pairings, person) match {
        case Some(pairing) =>
          val otherPerson =
            if (pairing.person1 == person) pairing.person2 else pairing.person1
          if (otherPerson == Me) true
          else {
            val pairings2 = pairings.filterNot(_ == pairing)
            loop(pairings2, otherPerson)
          }
        case None => false
      }
    }

    loop(pairings, person)
  }
}
