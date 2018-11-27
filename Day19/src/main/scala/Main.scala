import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    // val lines = Source.fromResource("test.txt").getLines().toList
    val (molecule, replacements) = parseLines(lines)
    val part1Answer = part1(molecule, replacements)
    println(s"part 1 answer: $part1Answer")
  }

  type Molecule = String
  type Molecules = List[String]
  type Replacement = (String, String)
  type Replacements = List[Replacement]

  private def parseLines(lines: Seq[String]): (Molecule, Replacements) = {
    val noBlankLines = lines.filterNot(_.isEmpty)
    val replacementLines = noBlankLines.init
    val molecule = noBlankLines.last
    val replacements = replacementLines
      .map(_.split(" => "))
      .map(arr => arr(0) -> arr(1))
      .toList
    (molecule, replacements)
  }

  private def part1(molecule: Molecule, replacements: Replacements): Int = {

    def replace(pos: Int, s1: String, s2: String): String =
      molecule.take(pos) ++ s2 ++ molecule.drop(pos + s1.length)

    def op(acc: Molecules, replacement: Replacement): Molecules = {
      val (s1, s2) = replacement
      @tailrec
      def loop(acc: Molecules, fromIndex: Int = 0): Molecules = {
        val pos = molecule.indexOf(s1, fromIndex)
        if (pos >= 0) {
          val acc2 = replace(pos, s1, s2) :: acc
          loop(acc2, pos + 1)
        } else acc
      }
      loop(acc)
    }

    val initialState: Molecules = List()
    val finalState = replacements.foldLeft(initialState)(op)
    finalState.distinct.length
  }
}
