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

  private def part1(ingredient: Seq[Ingredient]): Int = {
    val scores = for {
      amount1 <- 0 to 100
      amount2 <- 0 to 100
      if amount1 + amount2 == 100
    } yield calculateScore(ingredient.head, ingredient.last, amount1, amount2)
    scores.max
  }

  private def calculateScore(ingredient1: Ingredient,
                             ingredient2: Ingredient,
                             amount1: Int,
                             amount2: Int): Int = {
    def calculatePropertyScore(property: Ingredient => Int): Int = {
      val propertyScore =
        amount1 * property(ingredient1) +
        amount2 * property(ingredient2)
      Math.max(propertyScore, 0)
    }
    val c = calculatePropertyScore(_.capacity)
    val d = calculatePropertyScore(_.durability)
    val f = calculatePropertyScore(_.flavor)
    val t = calculatePropertyScore(_.texture)
    c * d * f * t
  }
}
