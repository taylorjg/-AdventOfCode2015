import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines()
    val boxes = parseLines(lines).toSeq
    part1(boxes)
    part2(boxes)
  }

  final case class Box(l: Int, w: Int, h: Int)

  private def parseLines(lines: Iterator[String]): Iterator[Box] =
    lines.map(parseLine)

  private final val LineRegex = """^(\d+)x(\d+)x(\d+)$""".r

  private def parseLine(line: String): Box = {
    line match {
      case LineRegex(l, w, h) => Box(l.toInt, w.toInt, h.toInt)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }
  }

  private def calculateWrappingPaper(box: Box): Int = {
    val l = box.l
    val w = box.w
    val h = box.h
    val surfaceArea = 2 * l * w + 2 * w * h + 2 * h * l
    val extra = List(l, w, h).sorted.take(2).product
    surfaceArea + extra
  }

  private def calculateRibbon(box: Box): Int = {
    val l = box.l
    val w = box.w
    val h = box.h
    val List(d1, d2) = List(l, w, h).sorted.take(2)
    val smallestPerimeter = d1 + d1 + d2 + d2
    val bow = l * w * h
    smallestPerimeter + bow
  }

  private def part1(boxes: Seq[Box]): Unit = {
    val wrappingPaper = boxes.map(calculateWrappingPaper).sum
    println(s"part 1 answer: $wrappingPaper")
  }

  private def part2(boxes: Seq[Box]): Unit = {
    val ribbon = boxes.map(calculateRibbon).sum
    println(s"part 2 answer: $ribbon")
  }
}
