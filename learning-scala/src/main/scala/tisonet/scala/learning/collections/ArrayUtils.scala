package tisonet.scala.learning.collections

object ArrayUtils {

  def swapAdjacent[T] (a: Array[T]) = {
    for (i <- 0 until (a.length - 1, 2)) {
      val t: T = a(i)
      a(i) = a(i + 1)
      a(i + 1) = t
    }
  }

  /**
    * Given an array of integers, produce a new array that contains all positive
    * values of the original array, in their original order, followed by all values that
    * are zero or negative, in their original order.
    */
  def orderByPositivity (a: Array[Int]) = {
    a.filter(el => el > 0) ++ a.filter(el => el == 0) ++ a.filter(el => el < 0)
  }

  def average (a: Array[Double]): Double = {
    a.sum / a.length
  }
}
