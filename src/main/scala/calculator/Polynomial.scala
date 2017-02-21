package calculator
import Math._
object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val b1 = b()
      val a1 = a()
      val c1 = c()
      b1*b1 - 4*a1*c1
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val b1 = b()
      val a1 = a()
      val c1 = c()
      val d1 = delta()
      if (d1 < 0) Set()
      else Set( (-b1 + Math.sqrt(d1))/(2*a1), (-b1 - Math.sqrt(d1))/(2*a1))
    }
  }
}
