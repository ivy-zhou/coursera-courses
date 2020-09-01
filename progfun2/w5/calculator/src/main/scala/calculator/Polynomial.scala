package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal({
      math.pow(b(), 2) - 4 * a() * c()
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      val d = delta()
      if (d > 0) {
        val negB = -b()
        val sqrtDelta = math.sqrt(d)
        val twoA = 2 * a()
        Set((negB + sqrtDelta) / twoA, (negB - sqrtDelta) / twoA)
      } else Set()
    })
  }
}
