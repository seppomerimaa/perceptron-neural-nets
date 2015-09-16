import scala.util.Random
import ListHelpers._

/**
 * Build Perceptrons. No frills.
 */
case class LabeledPoint(label: Double, point: List[Double])

class PerceptronBuilder(maxNumIter: Int, tol: Double) {
  val trainingRate = 0.001

  def build(points: List[LabeledPoint]): Perceptron = {
    val paddedPoints = points.map(p => new LabeledPoint(p.label, p.point :+ 1.0))

    def inner(numIter: Int, w: List[Double]): Perceptron = {
      val shuffled = Random.shuffle(paddedPoints)
      val misclassified = findMisclassifiedPoints(w, shuffled)
      val newW = misclassified.foldLeft(w)(updateW)
      val err = findMisclassifiedPoints(newW, shuffled).size / shuffled.size.toDouble
      if (numIter == maxNumIter || err < tol) {
        println(s"iter $numIter error: $err")
        new Perceptron(newW)
      } else {
        inner(numIter + 1, newW)
      }
    }
    inner(0, List.fill(points.head.point.size + 1)(0))
  }

  def updateW(oldW: List[Double], xy: (LabeledPoint, Double)): List[Double] = xy match {
    case (x, y) => oldW.plus(x.point.scaleBy(x.label * trainingRate))
    case _ => throw new RuntimeException("ummm")
  }

  def findMisclassifiedPoints(w: List[Double], points: List[LabeledPoint]): List[(LabeledPoint, Double)] = {
    val pointsAndGuessedLabels = points.map(p => (p, sign(p.point dot w)))
    val misclassified = pointsAndGuessedLabels.filter {
      case (p, guessedLabel) => p.label != guessedLabel
    }
    misclassified
  }

  def sign(x: Double): Double = if (x < 0) -1 else 1
}
