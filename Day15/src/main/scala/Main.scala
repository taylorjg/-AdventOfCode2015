import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input.txt").getLines().toList
    val ingredients = parseLines(lines)
    val part1Answer = part1(ingredients)
    println(s"part 1 answer: $part1Answer")
    val part2Answer = part2(ingredients)
    println(s"part 2 answer: $part2Answer")
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
      amount3 <- 0 to 100
      amount4 <- 0 to 100
      if amount1 + amount2 + amount3 + amount4 == 100
    } yield
      calculateScore(List(amount1 -> ingredients(0),
                          amount2 -> ingredients(1),
                          amount3 -> ingredients(2),
                          amount4 -> ingredients(3)),
                     None)
    scores.max
  }

  private def part2(ingredients: Seq[Ingredient]): Int = {
    val scores = for {
      amount1 <- 0 to 100
      amount2 <- 0 to 100
      amount3 <- 0 to 100
      amount4 <- 0 to 100
      if amount1 + amount2 + amount3 + amount4 == 100
    } yield
      calculateScore(List(amount1 -> ingredients(0),
                          amount2 -> ingredients(1),
                          amount3 -> ingredients(2),
                          amount4 -> ingredients(3)),
                     Some(500))
    scores.max
  }

  private def calculateScore(pairs: List[(Int, Ingredient)],
                             maybeTargetCalories: Option[Int]): Int = {
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
    val score = c * d * f * t
    maybeTargetCalories match {
      case Some(target) =>
        val calories = calculatePropertyScore(_.calories)
        if (calories == target) score else 0
      case None => score
    }
  }
}
