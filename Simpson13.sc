def simpson13 (f:Double => Double, lInferior : Int, lSuperior : Int, nIntervalos : Int) : Double = {
  val h = (lSuperior - lInferior) / nIntervalos * 1.0
  val xj = (j : Double ) => lInferior + (j * h)
  val f1 = (j : Double) => f(xj((2 * j) - 2)) + (4 * f(xj((2 * j) - 1))) + f(xj(2 * j))
  val sumatoria = Range(1, nIntervalos/2).map(f1(_)).sum
  (sumatoria * h) / 3
}

simpson13Extendido(funcion1, 3,5)

val funcion1 = (x: Double) => - Math.pow(x, 2) + ( 8 * x) - 12
simpson13(funcion1, 3,5, 2)

def simpson13Extendido (f:Double => Double, lInferior : Int, lSuperior : Int) : Double = {
  val n = 2 * (lSuperior - lInferior)
  val h = (lSuperior - lInferior) / n.toDouble
  val func1 = (x: Double) => lInferior + (x * h)
  val funciones = f(lInferior) + 4 * Range(1, n - 1, 2).map(func1(_)).sum +
    2 * Range(2, n - 2, 2).map(func1(_)).sum + f(lSuperior)

}
