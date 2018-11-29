object Main {

  def main(args: Array[String]): Unit = {
    val target = 36000000
    val part1Answer = part1(target)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(target)
    println(s"part 2 answer: $part2Answer")
  }

  private def part1(target: Int): Int =
    Stream.from(800000).find(factors(_).sum * 10 >= target).getOrElse(-1)

  private def part2(target: Int): Int =
    Stream.from(800000).find(n => filterFactors(n, factors(n)).sum * 11 >= target).getOrElse(-1)

  private def factors(n: Int): Seq[Int] =
    (1 to n).filter(n % _ == 0)

  private def filterFactors(n: Int, fs: Seq[Int]): Seq[Int] =
    fs.filter(n <= _ * 50)
}
