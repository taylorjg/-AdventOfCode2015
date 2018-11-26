import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val aunts = parseLines(lines)
    val part1Answer = findBestMatch(aunts)
    println(s"part 1 answer: $part1Answer")
  }

  private final val Analysis = List(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  private final val LineRegex =
    raw"""Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)""".r

  type Item = (String, Int)
  type Items = List[Item]
  final case class Aunt(number: Int, items: List[Item])

  private def parseLines(lines: List[String]): List[Aunt] =
    lines.map(parseLine)

  private def parseLine(line: String): Aunt =
    line match {
      case LineRegex(number, t1, q1, t2, q2, t3, q3) =>
        Aunt(number.toInt, List(t1 -> q1.toInt, t2 -> q2.toInt, t3 -> q3.toInt))
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def findBestMatch(aunts: List[Main.Aunt]): Int = {
    @tailrec
    def loop(remainingAunts: List[Aunt]): Int = {
      if (remainingAunts.length == 1) remainingAunts.head.number
      else {
        def filterAunts(acc: List[Aunt], item: Item): List[Aunt] =
          acc.filter { aunt =>
            aunt.items.find(_._1 == item._1) match {
              case Some(auntItem) => auntItem._2 == item._2
              case None           => true
            }
          }
        val remainingAunts2 = Analysis.foldLeft(remainingAunts)(filterAunts)
        loop(remainingAunts2)
      }
    }
    loop(aunts)
  }
}
