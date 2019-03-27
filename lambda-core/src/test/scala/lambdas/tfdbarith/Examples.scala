package lambdas
package tfdbarith

import tfdb.Lambda
import arithmetic.Arithmetic

case class ExamplesDB[P[_, _]]()(implicit L: Lambda[P], A: Forall[P, Arithmetic]) {
  import L._

  def ex: P[Unit, (Int => Int) => Int] =
    lam(app(vz[Unit, Int => Int])(A.apply.int(1)))

  def ex0: P[Unit, Int] =
    A.apply.add(A.apply.int(1))(A.apply.int(3))

  def ex1[E]: P[E, Int] =
    A.apply.add(A.apply.int(1))(A.apply.int(3))

  def ex2[E]: P[(Int, E), Int => Int] =
    lam(A.apply.add(vz[(Int, E), Int])(vs(vz)))

  def ex4[E]: P[(Int, (Int, E)), Int] =
    A.apply.add(vz[(Int, E), Int])(vs(vz))

  def ex3[E]: P[E, (Int => Int) => Int] =
    lam(A.apply.add(app(vz[E, Int => Int])(A.apply.int(1)))(A.apply.int(2)))
}
