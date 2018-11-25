import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    // val lines = Source.fromResource("test.txt").getLines().toList
    val deerData = parseLines(lines)
    // val part1Answer = part1(deerData, 1000)
    val part1Answer = part1(deerData, 2503)
    println(s"part 1 answer: $part1Answer")
  }

  final case class DeerData(name: String,
                            speed: Int,
                            flyingTime: Int,
                            restingTime: Int)

  private final val LineRegex =
    raw"""(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.""".r

  private def parseLines(lines: Seq[String]): Seq[DeerData] =
    lines.map(parseLine)

  private def parseLine(line: String): DeerData =
    line match {
      case LineRegex(name, speed, flyingTime, restingTime) =>
        DeerData(name, speed.toInt, flyingTime.toInt, restingTime.toInt)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def part1(deerData: Seq[DeerData], numSeconds: Int): Int = {
    val initialStates = deerData.map(dd => flying(dd, dd.flyingTime, 0))
    val v1 = Iterable.iterate(initialStates, numSeconds + 1)(advanceStates).toList
    val v2 = v1.last
    val v3 = v2.maxBy(s => s.td)
    v3.td
  }

  private def advanceStates(states: Seq[State]): Seq[State] =
    states.map(advanceState)

  private def advanceState(state: State): State =
    state match {
      case Flying(dd, r, td) =>
        if (r == 1) resting(dd, dd.restingTime, td + dd.speed)
        else flying(dd, r - 1, td + dd.speed)
      case Resting(dd, r, td) =>
        if (r == 1) flying(dd, dd.flyingTime, td)
        else resting(dd, r - 1, td)
    }

  sealed abstract class State(dd: DeerData, r: Int, val td: Int)
  final case class Flying(dd: DeerData, r: Int, override val td: Int)
      extends State(dd, r, td)
  final case class Resting(dd: DeerData, r: Int, override val td: Int)
      extends State(dd, r, td)

  private def flying(dd: DeerData, r: Int, td: Int): State = Flying(dd, r, td)
  private def resting(dd: DeerData, r: Int, td: Int): State = Resting(dd, r, td)
}
