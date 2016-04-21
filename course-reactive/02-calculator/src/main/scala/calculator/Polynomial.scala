package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal( b() * b() - 4 * a() * c() )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal( delta() match {
      case 0 => Set(-b() / 2 * a())
      case d if d < 0 => Set()
      case _ =>  Set(
        (-b() + Math.sqrt(delta())) / (2 * a()),
        (-b() - Math.sqrt(delta())) / (2 * a()))
    })
  }
}
