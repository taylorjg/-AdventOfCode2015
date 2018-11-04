import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").mkString
    part1(input)
    part2(input)
  }

  final case class Location(x: Int, y: Int)

  private def move(location: Location, ch: Char): Location =
    ch match {
      case '^' => Location(location.x, location.y + 1)
      case 'v' => Location(location.x, location.y - 1)
      case '<' => Location(location.x - 1, location.y)
      case '>' => Location(location.x + 1, location.y)
      case _   => location
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

  private def part2(input: String): Unit = {
    def loop(visited: Set[Location],
             santaLocations: Map[Int, Location],
             step: Int): Int = {
      if (!input.isDefinedAt(step)) visited.size
      else {
        val ch = input.charAt(step)
        val index = step % santaLocations.size
        val oldLocation = santaLocations(index)
        val newLocation = move(oldLocation, ch)
        val newSantaLocations = santaLocations.updated(index, newLocation)
        val newVisited = visited + newLocation
        loop(newVisited, newSantaLocations, step + 1)
      }
    }
    val start = Location(0, 0)
    val santas = 0 to 1
    val pairs = santas.map((_, start))
    val initialSantaLocations = Map(pairs:_*)
    val houses = loop(Set(start), initialSantaLocations, 0)
    println(s"part 2 answer: $houses")
  }
}
