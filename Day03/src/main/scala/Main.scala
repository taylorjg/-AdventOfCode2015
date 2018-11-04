import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").mkString
    part1(input)
    part2(input)
  }

  final case class Location(x: Int, y: Int)

  private def move(current: Location, ch: Char): Location =
    ch match {
      case '^' => Location(current.x, current.y + 1)
      case 'v' => Location(current.x, current.y - 1)
      case '<' => Location(current.x - 1, current.y)
      case '>' => Location(current.x + 1, current.y)
      case _   => current
    }

  private def part1(input: String): Unit = {
    def loop(visited: Set[Location], current: Location, step: Int): Int = {
      if (!input.isDefinedAt(step)) visited.size
      else {
        val ch = input.charAt(step)
        val newLocation = move(current, ch)
        loop(visited + newLocation, newLocation, step + 1)
      }
    }
    val start = Location(0, 0)
    val houses = loop(Set(start), start, 0)
    println(s"part 1 answer: $houses")
  }

  sealed abstract class Turn
  final case object SantasTurn extends Turn
  final case object RoboSantasTurn extends Turn

  private def part2(input: String): Unit = {
    def loop(visited: Set[Location],
             step: Int,
             turn: Turn,
             santasLocation: Location,
             roboSantasLocation: Location): Int = {
      if (!input.isDefinedAt(step)) visited.size
      else {
        val ch = input.charAt(step)
        val newStep = step + 1
        turn match {
          case SantasTurn =>
            val newLocation = move(santasLocation, ch)
            loop(visited + newLocation, newStep, RoboSantasTurn, newLocation, roboSantasLocation)
          case RoboSantasTurn =>
            val newLocation = move(roboSantasLocation, ch)
            loop(visited + newLocation, newStep, SantasTurn, santasLocation, newLocation)
        }
      }
    }
    val start = Location(0, 0)
    val houses = loop(Set(start), 0, SantasTurn, start, start)
    println(s"part 2 answer: $houses")
  }
}
