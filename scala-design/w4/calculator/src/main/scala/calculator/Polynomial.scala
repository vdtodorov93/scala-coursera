package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Var(b() * b() - 4 *  a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      Signal(comp(a, b, c, delta))
    }

    def comp(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Set[Double] = {

      if(delta() < 0) {
        Set()
      } else if(delta() == 0.0) {
        Set(-b() / (2 * a()))
      } else {
        Set(
          (-b() - Math.sqrt(delta())) / (2 * a()),
          (-b() + Math.sqrt(delta())) / (2 * a())
        )
    }
  }

//  def main(args: Array[String]): Unit = {
//    val a = Var(1.0)
//    val b = Var(4.0)
//    val c = Var(1.0)
//    val d = computeDelta(a, b, c)
//    val sol = computeSolutions(a, b, c, d)
//    println(d())
//    println(sol())
//    b() = 1
//    println(d())
//    println(sol())
//  }
}
