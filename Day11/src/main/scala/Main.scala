import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    val input = "cqjxjnds"
    val part1Answer = nextPassword(input)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = nextPassword(part1Answer)
    println(s"part 2 answer: $part2Answer")
  }

  private def nextPassword(pw: String): String = {
    @tailrec
    def loop(s: String): String = {
      val s2 = increment(s)
      if (meetsAllRequirements(s2)) s2
      else loop(s2)
    }
    loop(pw)
  }

  private def increment(pw: String): String = {
    @tailrec
    def loop(idx: Int, s: String): String = {
      val ch = s(idx) + 1
      if (ch <= 'z') setCharAt(s, ch.toChar, idx)
      else {
        if (idx == 0) {
          val len = s.length
          "a" * len
        } else loop(idx - 1, setCharAt(s, 'a', idx))
      }
    }
    loop(pw.indices.last, pw)
  }

  private def setCharAt(s: String, ch: Char, idx: Int): String = {
    val charArray = s.toCharArray
    charArray(idx) = ch
    charArray.mkString
  }

  private def meetsAllRequirements(pw: String): Boolean =
    requirement1(pw) && requirement2(pw) && requirement3(pw)

  private def requirement1(pw: String): Boolean = {
    def isStraightOf3(s: String): Boolean = {
      val ch1 = s(0)
      val ch2 = s(1)
      val ch3 = s(2)
      ch2 == ch1 + 1 && ch3 == ch1 + 2
    }
    val xs = pw.sliding(3)
    xs.exists(isStraightOf3)
  }

  private def requirement2(pw: String): Boolean =
    pw.indexOf('i') < 0 &&
      pw.indexOf('o') < 0 &&
      pw.indexOf('l') < 0

  private def requirement3(pw: String): Boolean = {
    def isMatchingPair(s: String): Boolean = s(0) == s(1)
    pw.sliding(2).toList.filter(isMatchingPair).distinct.length >= 2
  }
}
