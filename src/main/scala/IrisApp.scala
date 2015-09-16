import scala.io.Source

/**
 * Trains 3 Perceptrons, one for each Iris flower type. Then classifies the original training data by running each
 * feature set through each of the 3 Perceptrons and taking the label from the Perceptron that gives the data the best
 * (highest) score.
 */
object IrisApp extends App {
  override def main(args: Array[String]) = {
    val lines = Source.fromFile("src/main/resources/iris.txt").getLines().toList
    val hams = List("Iris-setosa", "Iris-versicolor", "Iris-virginica")
    val labelsAndPerceptrons = hams.map(trainOnOneType(lines, _))
    val labelsAndFeatures = lines.map(parseLine2)

    val numCorrect = labelsAndFeatures.map { case (trueL, f) =>
        val guessAndScore = labelsAndPerceptrons.map { case (l, p) =>
          (l, p.unweightedClassify(f))
        }
        val bestGuess = guessAndScore.maxBy(_._2)._1
        println(s"true: $trueL guess: $bestGuess")
        if (bestGuess == trueL) 1 else 0
    }.sum
    val err = 1.0 - numCorrect / lines.size.toDouble
    println(s"Multiclassification error rate: $err")
  }

  def trainOnOneType(lines: Seq[String], ham: String): (String, Perceptron) = {
    val ps = lines.map(parseLine(_, ham))

    println(s"Training a perceptron to classify $ham / Other")
    val hebb = new PerceptronBuilder(10000, 0.001)
    val result = hebb.build(ps.toList)
    println(result.w)
    (ham, result)
  }

  def parseLine(l: String, ham: String): LabeledPoint = {
    val f = l.split(",")
    val p = List(f(0).toDouble, f(1).toDouble, f(2).toDouble, f(3).toDouble)
    val label = if (f(4) == ham) 1 else -1
    new LabeledPoint(label, p)
  }

  def parseLine2(l: String): (String, List[Double]) = {
    val f = l.split(",")
    (f(4), List(f(0).toDouble, f(1).toDouble, f(2).toDouble, f(3).toDouble))
  }
}
