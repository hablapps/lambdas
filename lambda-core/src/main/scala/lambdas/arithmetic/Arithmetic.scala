package lambdas
package arithmetic

trait Arithmetic[P[_]] {

  def * : P[(Int, Int) => Int]

  def + : P[(Int, Int) => Int]
}

object Arithmetic {
  implicit val ArithmeticId   = semantics.ArithmeticId
  implicit val ArithmeticShow = semantics.ShowArithFun
}
