import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    val input = "1321131112"
    val part1Answer = part1(input)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(input)
    println(s"part 2 answer: $part2Answer")
  }

  private def part1(input: String): Int =
    List.iterate(input, 41)(generateNextString).last.length

  private def generateNextString(s: String): String = {
    @tailrec
    def loop(remaining: String, acc: String): String = {
      if (remaining.isEmpty) acc
      else {
        val ch = remaining.head
        val run = remaining.takeWhile(_ == ch)
        val count = run.length
        val sequence = s"$count$ch"
        loop(remaining.substring(count), acc + sequence)
      }
    }
    loop(s, "")
  }

  private def part2(input: String): Int = {
    val initialPairs = toPairs(input)
    val finalPairs = List.iterate(initialPairs, 41)(generateNextPairs).last
    pairsLength(finalPairs)
  }

  type Count = Int
  type Digit = Int
  type Pairs = List[(Count, Digit)]

  private def toPairs(input: String): Pairs = {
    @tailrec
    def loop(remaining: String, acc: Pairs): Pairs = {
      if (remaining.isEmpty) acc
      else {
        val ch = remaining.head
        val run = remaining.takeWhile(_ == ch)
        val count = run.length
        val pair = (count, ch - '0')
        loop(remaining.substring(count), pair :: acc)
      }
    }
    loop(input, List()).reverse
  }

  // 1 becomes 11
  // [(1, 1)] => [(2, 1)]

  // 11 becomes 21
  // [(2, 1)] => [(1, 2), (1, 1)]

  // 21 becomes 1211
  // [(1, 2), (1, 1)] => [(1,1), (1,2), (2,1)]>

  // 1211 becomes 111221
  // [(1,1), (1,2), (2,1)] => [(3,1), (2,2), (1,1)]>

  // 111221 becomes 312211
  // [(3,1), (2,2), (1,1)] => [(1,3), (1,1), (2,2), (2,1)]

  // ----------------------------------------

  // [(1, 1)] => [(2, 1)]
  // [(2, 1)] => [(1, 2), (1, 1)]
  // [(1, 2), (1, 1)] => [(1,1), (1,2), (2,1)]
  // [(1,1), (1,2), (2,1)] => [(3,1), (2,2), (1,1)]
  // [(3,1), (2,2), (1,1)] => [(1,3), (1,1), (2,2), (2,1)]

  private def generateNextPairs(pairs: Pairs): Pairs = {
    pairs
  }

  private def pairsLength(pairs: Pairs): Int =
    pairs.map(_._1).sum
}
