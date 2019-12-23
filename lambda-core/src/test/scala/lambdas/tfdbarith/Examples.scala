package lambdas
package tfdbarith

import tfdb.Lambda
import arithmetic._
import IntType.Implicits._
import ArrowType.Implicits._

case class ExamplesDB[Type[_]: ArrowType: IntType, P[_, _]]()(
    implicit L: Lambda[Type, P],
    A: Forall[P, Arithmetic]
) {
  import L._

  def ex: P[Unit, (Num => Num) => Num] =
    lam(app(vz[Unit, Num => Num])(A.apply.int(1.bd)))

  def ex0: P[Unit, Num] =
    A.apply.add(A.apply.int(1.bd))(A.apply.int(3.bd))

  def ex1[E]: P[E, Num] =
    A.apply.add(A.apply.int(1.bd))(A.apply.int(3.bd))

  def ex2[E]: P[(Num, E), Num => Num] =
    lam(A.apply.add(vz[(Num, E), Num])(vs(vz)))

  def ex4[E]: P[(Num, (Num, E)), Num] =
    A.apply.add(vz[(Num, E), Num])(vs(vz))

  def ex3[E]: P[E, (Num => Num) => Num] =
    lam(A.apply.add(app(vz[E, Num => Num])(A.apply.int(1.bd)))(A.apply.int(2.bd)))
}
