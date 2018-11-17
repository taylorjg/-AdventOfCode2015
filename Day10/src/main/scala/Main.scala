import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    val input = "1321131112"
    // val part1Answer = part1(input)
    // println(s"part 1 answer: $part1Answer")
    // val part2Answer = part2(input)
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
    // val finalPairs = List.iterate(initialPairs, 7)(generateNextPairs).last
    pairsLength(finalPairs)
  }

  type Count = Int
  type Digit = Char
  type Pair = (Count, Char)
  type Pairs = List[Pair]

  private def toPairs(input: String): Pairs = {
    @tailrec
    def loop(remaining: String, acc: Pairs): Pairs = {
      if (remaining.isEmpty) acc
      else {
        val ch = remaining.head
        val run = remaining.takeWhile(_ == ch)
        val count = run.length
        val pair = (count, ch)
        loop(remaining.substring(count), pair :: acc)
      }
    }
    loop(input, List()).reverse
  }

  // 1 becomes 1-1
  // [(1, 1)] => [(2, 1)]

  // 1-1 becomes 2-1
  // [(2, 1)] => [(1, 2), (1, 1)]

  // 2-1 becomes 12-11
  // [(1, 2), (1, 1)] => [(1,1), (1,2), (2,1)]

  // 12-11 becomes 11-12-21
  // [(1,1), (1,2), (2,1)] => [(3,1), (2,2), (1,1)]
  // [(1,1), (1,2)] "12" => "1112" => [(3,1), (1,2)]
  // [(1,2), (2,1)] "211" => "1221" => [(1,1), (2,2), (1,1)]

  // 11-12-21 becomes 31-22-11
  // [(3,1), (2,2), (1,1)] => [(1,3), (1,1), (2,2), (2,1)]

  // ----------------------------------------

  // 1 => [(1, 1)]
  // [(1, 1)] => [(2, 1)]
  // [(2, 1)] => [(1, 2), (1, 1)]
  // [(1, 2), (1, 1)] => [(1,1), (1,2), (2,1)]
  // [(1,1), (1,2), (2,1)] => [(3,1), (2,2), (1,1)]
  // [(3,1), (2,2), (1,1)] => [(1,3), (1,1), (2,2), (2,1)]

  private def generateNextPairs(pairs: Pairs): Pairs = {
    // println(s"[generateNextPairs] $pairs")
    pairs.foldLeft(List[Pair]()) {
      case (acc, pair) =>
        val nextString = generateNextString(pair._2.toString * pair._1)
        val nextPairs = toPairs(nextString)
        val firstNextPair = nextPairs.head
        acc.lastOption match {
          case Some(lastPair) if lastPair._2 == firstNextPair._2 =>
            val combinedPair = (lastPair._1 + firstNextPair._1, lastPair._2)
            acc.init ++ List(combinedPair) ++ nextPairs.tail
          case _ =>
            acc ++ nextPairs
        }
    }
  }

  private def pairsLength(pairs: Pairs): Int =
    pairs.map(_._1).sum
}
