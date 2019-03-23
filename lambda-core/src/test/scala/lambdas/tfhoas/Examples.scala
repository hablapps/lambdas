package lambdas
package tfhoas

import arithmetic.Arithmetic

case class Examples[P[_]]()(implicit L: Lambda[P], A: Arithmetic[P]) {

  import L._
  import A._

  def ex1: P[Int] =
    add(int(1))(int(3))

  // def ex1[E]: P[E, Int] =
  //   add(int(1))(int(3))

  // def ex2[E]: P[(Int, E), Int => Int] =
  //   lam(add(vz[(Int, E), Int])(vs(vz)))

  // def ex4[E]: P[(Int, (Int, E)), Int] =
  //   add(vz[(Int, E), Int])(vs(vz))

  // td3 = lam (add (app z (int 1)) (int 2))
  def ex3: P[(Int => Int) => Int] =
    lam(f => add(app(f)(int(1)))(int(2)))
}
