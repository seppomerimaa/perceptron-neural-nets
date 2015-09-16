import ListHelpers._

class Perceptron(val w: List[Double]) {
  def classify(x: List[Double]): Double = {
    sign(w dot x :+ 1.0) // don't forget to pad that last guy...
  }

  def unweightedClassify(x: List[Double]): Double = {
    w dot x :+ 1.0
  }

  def sign(x: Double): Double = if (x < 0) -1 else 1
}
