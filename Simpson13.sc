def simpson13 (f:Double => Double, lInferior : Int, lSuperior : Int, nIntervalos : Int) : Double = {
  val h = (lSuperior - lInferior) / nIntervalos * 1.0

  val xj = (j : Double ) => lInferior + (j * h)
  val f1 = (j : Double) => f(xj((2 * j)-2)) + (4 * f(xj((2 * j) - 1))) + f(xj(2 * j))
  val sumatoria = (1 to nIntervalos/2).map(f1(_)).sum
  (sumatoria * h) / 3
}


