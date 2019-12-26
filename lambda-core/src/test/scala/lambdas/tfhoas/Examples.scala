package lambdas
package tfhoas

import arithmetic._
import ArrowType.Implicits._
import IntType.Implicits._

case class Examples[Type[_]: ArrowType: IntType, P[_]]()(
    implicit
    L: Lambda[Type, P],
    A: Arithmetic[P]
) {

  import L._
  import A._

  def ex1: P[Num] =
    A.add(int(1.bd))(int(3.bd))

  def ex3: P[(Num => Num) => Num] =
    lam { f =>
      A.add(app(f)(int(1.bd)))(int(2.bd))
    }

  def ex2: P[(Num => Num) => Num => Num] =
    lam((f: P[Num => Num]) => lam((n: P[Num]) => app(f)(n)))

}
