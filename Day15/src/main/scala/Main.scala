import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    // val lines = Source.fromResource("input.txt").getLines().toList
    val lines = Source.fromResource("test.txt").getLines().toList
    val ingredients = parseLines(lines)
    val part1Answer = part1(ingredients)
    println(s"part 1 answer: $part1Answer")
  }

  private final val LineRegex =
    raw"""(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (\d+)""".r

  final case class Ingredient(name: String,
                              capacity: Int,
                              durability: Int,
                              flavor: Int,
                              texture: Int,
                              calories: Int)

  private def parseLines(lines: Seq[String]): Seq[Ingredient] =
    lines.map(parseLine)

  private def parseLine(line: String): Ingredient =
    line match {
      case LineRegex(name, capacity, durability, flavor, texture, calories) =>
        Main.Ingredient(name,
                        capacity.toInt,
                        durability.toInt,
                        flavor.toInt,
                        texture.toInt,
                        calories.toInt)
      case _ => throw new Exception(s"Failed to parse line, '$line'.")
    }

  private def part1(ingredients: Seq[Ingredient]): Int = {
    val scores = for {
      amount1 <- 0 to 100
      amount2 <- 0 to 100
      if amount1 + amount2 == 100
    } yield
      calculateScore(
        List(amount1 -> ingredients.head, amount2 -> ingredients.last))
    scores.max
  }

  private def calculateScore(pairs: List[(Int, Ingredient)]): Int = {
    def calculatePropertyScore(property: Ingredient => Int): Int = {
      val propertyScores = pairs.map {
        case (amount, ingredient) => amount * property(ingredient)
      }
      Math.max(propertyScores.sum, 0)
    }
    val c = calculatePropertyScore(_.capacity)
    val d = calculatePropertyScore(_.durability)
    val f = calculatePropertyScore(_.flavor)
    val t = calculatePropertyScore(_.texture)
    c * d * f * t
  }
}