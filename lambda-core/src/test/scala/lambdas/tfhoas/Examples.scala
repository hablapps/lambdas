package lambdas
package tfhoas

import arithmetic.{ Arithmetic, IntType }, ArrowType.Implicits._, IntType.Implicits._

case class Examples[Type[_]: ArrowType: IntType, P[_]]()(
    implicit
    L: Lambda[Type, P],
    A: Arithmetic[P]
) {

  import L._
  import A._

  def ex1: P[Int] =
    add(int(1))(int(3))

  def ex3: P[(Int => Int) => Int] =
    lam { f =>
      add(app(f)(int(1)))(int(2))
    }
}
