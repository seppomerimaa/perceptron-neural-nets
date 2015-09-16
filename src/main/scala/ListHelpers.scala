/**
 * The road to implicit hell is paved with laziness...
 */
object ListHelpers {

  implicit class ListShadow(thisList: List[Double]) {
    def dot(thatList: List[Double]): Double = {
      (thisList zip thatList).map {
        case (v1: Double, v2: Double) => v1 * v2
      }.sum
    }

    def scaleBy(scalar: Double): List[Double] = {
      thisList.map(_ * scalar)
    }

    def plus(thatList: List[Double]): List[Double] = {
      (thisList, thatList).zipped.map(_+_)
    }
  }
}
