import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    // val lines = Source.fromResource("test1.txt").getLines().toList
    // val lines = Source.fromResource("test2.txt").getLines().toList
    val (molecule, replacements) = parseLines(lines)
    val part1Answer = part1(molecule, replacements)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(molecule, replacements)
    println(s"part 2 answer: $part2Answer")
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

  private def part1(molecule: Molecule, replacements: Replacements): Int =
    generateMolecules(replacements)(molecule).length

  // TODO: try depth-first instead of breadth-first and/or use Streams ?
  private def part2(targetMolecule: Molecule,
                    replacements: Replacements): Int = {
    @tailrec
    def loop(molecules: Molecules, steps: Int = 1): Int = {
      // println(s"molecules: $molecules; steps: $steps")
      if (molecules.contains(targetMolecule)) steps
      else {
        val molecules2 =
          molecules.flatMap(generateMolecules(replacements)).distinct
        val molecules3 = molecules2.filterNot(_.length > targetMolecule.length)
        // molecules2.foreach(println)
        loop(molecules3, steps + 1)
      }
    }
    val startingPoints = replacements.filter(_._1 == "e").map(_._2)
    loop(startingPoints)
  }

  private def generateMolecules(replacements: Replacements)(
      molecule: Molecule): Molecules = {

    def doReplacement(pos: Int, s1: String, s2: String): String =
      molecule.take(pos) ++ s2 ++ molecule.drop(pos + s1.length)

    def doAllReplacementsFor(outerAcc: Molecules,
                             replacement: Replacement): Molecules = {
      val (s1, s2) = replacement
      @tailrec
      def loop(innerAcc: Molecules, fromIndex: Int = 0): Molecules = {
        val pos = molecule.indexOf(s1, fromIndex)
        if (pos < 0) innerAcc
        else {
          val molecule2 = doReplacement(pos, s1, s2)
          val innerAcc2 = molecule2 :: innerAcc
          loop(innerAcc2, pos + 1)
        }
      }
      loop(outerAcc)
    }

    val initialState: Molecules = Nil
    val finalState = replacements.foldLeft(initialState)(doAllReplacementsFor)
    finalState.distinct
  }
}
