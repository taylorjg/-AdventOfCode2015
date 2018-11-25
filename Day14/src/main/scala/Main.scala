import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    // val lines = Source.fromResource("test.txt").getLines().toList
    val deerData = parseLines(lines)
    // val numSeconds = 1000
    val numSeconds = 2503
    val part1Answer = part1(deerData, numSeconds)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(deerData, numSeconds)
    println(s"part 2 answer: $part2Answer")
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
    val initialStates = deerData.map(dd => flying(dd, dd.flyingTime, 0, 0))
    Iterable
      .iterate(initialStates, numSeconds + 1)(advanceStates)
      .last
      .maxBy(_.td)
      .td
  }

  private def advanceStates(states: Seq[State]): Seq[State] =
    states.map(advanceState)

  private def advanceState(state: State): State =
    state match {
      case Flying(dd, r, td, pts) =>
        if (r == 1) resting(dd, dd.restingTime, td + dd.speed, pts)
        else flying(dd, r - 1, td + dd.speed, pts)
      case Resting(dd, r, td, pts) =>
        if (r == 1) flying(dd, dd.flyingTime, td, pts)
        else resting(dd, r - 1, td, pts)
    }

  sealed abstract class State(dd: DeerData, r: Int, val td: Int, val pts: Int)
  final case class Flying(dd: DeerData,
                          r: Int,
                          override val td: Int,
                          override val pts: Int)
      extends State(dd, r, td, pts)
  final case class Resting(dd: DeerData,
                           r: Int,
                           override val td: Int,
                           override val pts: Int)
      extends State(dd, r, td, pts)

  private def flying(dd: DeerData, r: Int, td: Int, pts: Int): State =
    Flying(dd, r, td, pts)
  private def resting(dd: DeerData, r: Int, td: Int, pts: Int): State =
    Resting(dd, r, td, pts)

  private def part2(deerData: Seq[DeerData], numSeconds: Int): Int = {
    val initialStates = deerData.map(dd => flying(dd, dd.flyingTime, 0, 0))
    Iterable
      .iterate(initialStates, numSeconds + 1)(advanceStates2)
      .last
      .maxBy(_.pts)
      .pts
  }

  private def advanceStates2(states: Seq[State]): Seq[State] = {
    val states2 = states.map(advanceState)
    val tdMax = states2.maxBy(_.td).td
    states2.map(incrementLeaders(tdMax))
  }

  private def incrementLeaders(tdMax: Int)(state: State): State = {
    def addPointIfLeader(td: Int, pts: Int): Int =
      if (td == tdMax) pts + 1 else pts
    state match {
      case Flying(dd, r, td, pts) =>
        flying(dd, r, td, addPointIfLeader(td, pts))
      case Resting(dd, r, td, pts) =>
        resting(dd, r, td, addPointIfLeader(td, pts))
    }
  }
}
