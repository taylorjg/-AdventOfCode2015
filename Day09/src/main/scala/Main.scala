import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val distances = parseLines(lines)
    val part1Answer = part1(distances)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(distances)
    println(s"part 2 answer: $part2Answer")
  }

  final case class Distance(location1: String, location2: String, miles: Int)
  final case class Route(locations: Seq[String], miles: Int)

  private final val LineRegex = raw"""(\w+) to (\w+) = (\d+)""".r

  private def parseLines(lines: Seq[String]): Seq[Distance] =
    lines.map(parseLine)

  private def parseLine(line: String): Distance =
    line match {
      case LineRegex(l1, l2, m) => Distance(l1, l2, m.toInt)
      case _                    => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def part1(distances: Seq[Distance]): Int =
    minMaxDistances(distances)._1

  private def part2(distances: Seq[Distance]): Int =
    minMaxDistances(distances)._2

  private def minMaxDistances(distances: Seq[Distance]): (Int, Int) = {
    val location1s = distances.map(_.location1)
    val location2s = distances.map(_.location2)
    val locations = (location1s ++ location2s).distinct
    val possibleRoutes = calcPossibleRoutes(distances, locations)
    val miles = possibleRoutes.map(_.miles)
    (miles.min, miles.max)
  }

  private def calcPossibleRoutes(distances: Seq[Distance],
                                 locations: Seq[String]): Seq[Route] = {
    def totalDistance(locations: Seq[String]): Option[Int] = {
      def lookupDistance(l1: String, l2: String): Option[Distance] =
        distances.find( d =>
            (d.location1 == l1 && d.location2 == l2) ||
            (d.location1 == l2 && d.location2 == l1))
      val miles = locations.sliding(2).toList.map { ls =>
        val l1 = ls.head
        val l2 = ls.tail.head
        lookupDistance(l1, l2).map(_.miles)
      }
      if (miles.forall(_.isDefined)) Some(miles.flatten.sum) else None
    }
    val permutations = locations.permutations.toList
    permutations.flatMap(locations =>
      totalDistance(locations).map(miles => Route(locations, miles)))
  }
}
