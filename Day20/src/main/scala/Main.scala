object Main {

  def main(args: Array[String]): Unit = {
    val target = 36000000
    // val target = 120
    val part1Answer = part1(target)
    println(s"part 1 answer: $part1Answer")
  }

  private def part1(target: Int): Int =
    Stream.from(800000).find(factors(_).sum * 10 >= target).getOrElse(-1)

  private def factors(n: Int): Seq[Int] =
    (1 to n).filter(n % _ == 0)
}
