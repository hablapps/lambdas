package lambdas
package arithmetic

trait Arithmetic[P[_]] {

  def int(i: Int): P[Int]

  def add(i1: P[Int])(i2: P[Int]): P[Int]

  def * : P[(Int, Int) => Int]

  def + : P[(Int, Int) => Int]
}

object Arithmetic {

  def apply[P[_]](implicit A: Arithmetic[P]) = A

  implicit val ArithmeticId   = semantics.ArithmeticId
  implicit val ArithmeticShow = semantics.ShowArithFun
}
