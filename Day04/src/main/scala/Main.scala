import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.annotation.tailrec

object Main {

  private final val SecretKey = "bgvyzdsv"

  def main(args: Array[String]): Unit = {
    part1(SecretKey)
    part2(SecretKey)
  }

  private def part1(secretKey: String): Unit = {
    val num = loop(5, secretKey, 1)
    println(s"part 1 answer: $num")
  }

  private def part2(secretKey: String): Unit = {
    val num = loop(6, secretKey, 254575)
    println(s"part 2 answer: $num")
  }

  def loop(numLeadingZeros: Int, secretKey: String, startingNumber: Int): Int = {
    val prefix = "0" * numLeadingZeros
    @tailrec
    def innerLoop(n: Int): Int = {
      val hash = calculateHash(secretKey, n)
      if (hash.startsWith(prefix)) n else innerLoop(n + 1)
    }
    innerLoop(startingNumber)
  }

  private final val MD5MessageDigest = MessageDigest.getInstance("MD5")

  private def calculateHash(s: String, n: Int): String = {
    val b = s"$s$n".getBytes(StandardCharsets.UTF_8)
    MD5MessageDigest.update(b, 0, b.length)
    MD5MessageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
