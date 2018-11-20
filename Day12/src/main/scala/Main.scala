import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._

object Main {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.json").mkString
    val jvalue = parse(input)
    val part1Answer = part1(jvalue)
    println(s"Part 1 answer: $part1Answer")
  }

  private def part1(jvalue: JValue): Int = {
    def loop(acc: Int, jv: JValue): Int =
      jv match {
        case JInt(n)        => acc + n.toInt
        case JArray(jvs)    => acc + jvs.foldLeft(0)(loop)
        case JObject(pairs) => acc + pairs.map(_._2).foldLeft(0)(loop)
        case _              => acc
      }
    loop(0, jvalue)
  }
}
