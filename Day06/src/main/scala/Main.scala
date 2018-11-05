import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toSeq
    val instructions = parseLines(lines)
    part1(instructions)
    part2(instructions)
  }

  sealed abstract class Disposition
  final case object TurnOn extends Disposition
  final case object TurnOff extends Disposition
  final case object Toggle extends Disposition

  final case class Coords(x: Int, y: Int)
  final case class Rect(corner1: Coords, corner2: Coords)
  final case class Instruction(disposition: Disposition, rect: Rect)

  private def parseLines(lines: Seq[String]): Seq[Instruction] =
    lines.map(parseLine)

  private final val LineRegex =
    """^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$""".r

  private def stringToDisposition(s: String): Disposition =
    s match {
      case "turn on"  => TurnOn
      case "turn off" => TurnOff
      case "toggle"   => Toggle
      case _          => throw new Exception(s"Failed to convert '$s' to a Disposition.")
    }

  type Grid = Map[Coords, Int]
  type ApplyInstructionToLocation = (Grid, Disposition, Coords) => Grid

  private def applyInstructionToLocation1(grid: Grid,
                                          disposition: Disposition,
                                          location: Coords): Grid = {
    val newState =
      disposition match {
        case TurnOn  => 1
        case TurnOff => 0
        case Toggle  => if (grid.getOrElse(location, 0) == 0) 1 else 0
      }
    grid.updated(location, newState)
  }

  private def applyInstructionToLocation2(grid: Grid,
                                          disposition: Disposition,
                                          location: Coords): Grid = {
    val oldBrightness = grid.getOrElse(location, 0)
    val newBrightness =
      disposition match {
        case TurnOn  => oldBrightness + 1
        case TurnOff => Math.max(oldBrightness - 1, 0)
        case Toggle  => oldBrightness + 2
      }
    grid.updated(location, newBrightness)
  }

  private def rectLocations(rect: Rect): Seq[Coords] =
    for {
      x <- rect.corner1.x to rect.corner2.x
      y <- rect.corner1.y to rect.corner2.y
    } yield Coords(x, y)

  private def applyInstruction(
      applyInstructionToLocation: ApplyInstructionToLocation)(
      grid: Grid,
      instruction: Instruction): Grid = {
    val locations = rectLocations(instruction.rect)
    locations.foldLeft(grid) {
      case (currentGrid, location) =>
        applyInstructionToLocation(currentGrid,
                                   instruction.disposition,
                                   location)
    }
  }

  private def applyInstructions(
      grid: Grid,
      instructions: Seq[Instruction],
      applyInstructionToLocation: ApplyInstructionToLocation): Grid =
    instructions.foldLeft(grid)(applyInstruction(applyInstructionToLocation))

  private def countOnLights(grid: Grid): Int =
    grid.values.count(_ == 1)

  private def totalBrightness(grid: Grid): Int =
    grid.values.sum

  private def parseLine(line: String): Instruction = {
    line match {
      case LineRegex(d, x1, y1, x2, y2) =>
        val disposition = stringToDisposition(d)
        val corner1 = Coords(x1.toInt, y1.toInt)
        val corner2 = Coords(x2.toInt, y2.toInt)
        val rect = Rect(corner1, corner2)
        Instruction(disposition, rect)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }
  }

  private def part1(instructions: Seq[Instruction]): Unit = {
    val initialGrid: Grid = Map()
    val finalGrid = applyInstructions(initialGrid, instructions, applyInstructionToLocation1)
    val answer = countOnLights(finalGrid)
    println(s"part 1 answer: $answer")
  }

  private def part2(instructions: Seq[Instruction]): Unit = {
    val initialGrid: Grid = Map()
    val finalGrid = applyInstructions(initialGrid, instructions, applyInstructionToLocation2)
    val answer = totalBrightness(finalGrid)
    println(s"part 2 answer: $answer")
  }
}
