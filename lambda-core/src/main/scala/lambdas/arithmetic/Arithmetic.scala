package lambdas
package arithmetic

trait Arithmetic[P[_]] {

  def int(i: Int): P[Int]

  def add(i1: P[Int])(i2: P[Int]): P[Int]

  def * : P[(Int, Int) => Int]

  def + : P[(Int, Int) => Int]
}

object Arithmetic extends LPI {

  def apply[P[_]](implicit A: Arithmetic[P]) = A

  implicit val ArithmeticId        = semantics.ArithmeticId
  implicit val ArithmeticShow      = semantics.ShowArithFun
  implicit val ArithmeticShowB     = semantics.ShowBArith
  implicit val ArithmeticFunction1 = semantics.Function1Arith
}

trait LPI {

  implicit def ArithForall[E, P[_, _]](implicit FA: ForAll[P, Arithmetic]): Arithmetic[P[E, ?]] =
    FA[E]
}
