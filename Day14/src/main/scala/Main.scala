import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val fileName = "input.txt" // "test.txt"
    val raceLength = 2503 // 1000
    val lines = Source.fromResource(fileName).getLines().toList
    val dds = parseLines(lines)
    val part1Answer = reindeerRace(dds, raceLength, _.td)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = reindeerRace(dds, raceLength, _.pts)
    println(s"part 2 answer: $part2Answer")
  }

  final case class DeerData(name: String,
                            kms: Int,
                            flyingTime: Int,
                            restingTime: Int)

  private final val LineRegex =
    raw"""(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.""".r

  private def parseLines(lines: Seq[String]): Seq[DeerData] =
    lines.map(parseLine)

  private def parseLine(line: String): DeerData =
    line match {
      case LineRegex(name, kms, flyingTime, restingTime) =>
        DeerData(name, kms.toInt, flyingTime.toInt, restingTime.toInt)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
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

  private def reindeerRace(dds: Seq[DeerData],
                           raceLength: Int,
                           pluck: State => Int): Int = {
    val initialStates = dds.map(dd => flying(dd, dd.flyingTime, 0, 0))
    val winner = Iterable
      .iterate(initialStates, raceLength + 1)(advanceStates)
      .last
      .maxBy(pluck)
    pluck(winner)
  }

  private def advanceStates(states: Seq[State]): Seq[State] = {
    val states2 = states.map(advanceState)
    val tdMax = states2.maxBy(_.td).td
    states2.map(addPointToLeaders(tdMax))
  }

  private def advanceState(state: State): State =
    state match {
      case Flying(dd, r, td, pts) =>
        if (r == 1) resting(dd, dd.restingTime, td + dd.kms, pts)
        else flying(dd, r - 1, td + dd.kms, pts)
      case Resting(dd, r, td, pts) =>
        if (r == 1) flying(dd, dd.flyingTime, td, pts)
        else resting(dd, r - 1, td, pts)
    }

  private def addPointToLeaders(tdMax: Int)(state: State): State = {
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
