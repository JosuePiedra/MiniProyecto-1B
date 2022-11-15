def simpson13 (f:Double => Double, lInferior : Int, lSuperior : Int, nIntervalos : Int) : Double = {
  val h = (lSuperior - lInferior) / nIntervalos * 1.0
  val xj = (j: Double) => lInferior + (j * h)
  val f1 = (j: Double) => f(xj((2 * j) - 2)) + (4 * f(xj((2 * j) - 1))) + f(xj(2 * j))
  val sumatoria = (1 to nIntervalos / 2).map(f1(_)).sum
  (sumatoria * h) / 3
}

def simpson13Extendido (f:Double => Double, lInferior : Int, lSuperior : Int) : Double = {
  val n = 2 * (lSuperior - lInferior)
  val h = (lSuperior - lInferior) / n.toDouble
  val func1 = (x: Double) => f(lInferior + (x * h))
  val funciones = f(lInferior) + 4 * (1 until n by 2).map(func1(_)).sum +
    2 * (2 to n - 2 by 2).map(func1(_)).sum + f(lSuperior)

  (funciones * h) / 3.0

}

def f1(x:Double) = -math.pow(x,2) + (8 * x) - 12
simpson13(f1, 3, 5, 2)
simpson13Extendido(f1, 3, 5)


def f2(x:Double) = 3 * math.pow(x,2)
simpson13(f2, 0, 2, 2)
simpson13Extendido(f2, 0, 2)

def f3(x:Double) = x + (2 * math.pow(x,2)) -  math.pow(x,3) + (5 * math.pow(x,4))
simpson13(f3, -1, 1, 2)
simpson13Extendido(f3, -1, 1)


def f4(x:Double) = ((2*x)+1) / (math.pow(x,2) + x)
simpson13(f4, 1, 2, 2)
simpson13Extendido(f4, 1, 2)

def f5(x:Double) = Math.exp(x)
simpson13(f5, 0, 1, 2)
simpson13Extendido(f5, 0, 1)

def f6(x:Double) = 1 / math.sqrt(1 + math.pow(x,6))
simpson13(f6, 2, 3, 2)
simpson13Extendido(f6, 2, 3)

def f7(x:Double) = 1 / 1 + math.pow(x,2)
simpson13(f7, 0, 1, 2)
simpson13Extendido(f2, 0, 1)


