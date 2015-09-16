import ListHelpers._
/**
 * Generates some points around a particular plane and builds a Perceptron for them.
 */
object BasicApp {
  def main(args: Array[String]) = {
    println("Generating fake data for the equation 2x + 3y + 4z = 7 in range -10:10")
    val trueW = List(2.0, 3.0, 4.0, -7.0)

    val examples = for {
      x <- -10 to 10
      y <- -10 to 10
      z <- -10 to 10
    } yield {
      val point = List[Double](x, y, z)
      val s = sign(trueW.dot(point :+ 1.0))
      new LabeledPoint(s, point)
    }

    println("Training a Perceptron...")
    val hebb = new PerceptronBuilder(10000, 0.001)
    val result = hebb.build(examples.toList)
    println(result.w)
  }

  def sign(x: Double): Double = if (x < 0) -1 else 1

}
