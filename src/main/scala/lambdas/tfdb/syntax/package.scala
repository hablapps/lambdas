package lambdas
package tfdb

package object syntax {

  implicit def int[P[_, _], E](i: Int)(implicit L: Lambda[P]): P[E, Int] =
    L.int[E](i)

  implicit class AddOp[P[_, _], E](e1: P[E, Int])(implicit L: Lambda[P]) {
    def +(e2: P[E, Int]): P[E, Int] =
      L.add(e1, e2)
  }

  def add[P[_, _], E](e1: P[E, Int], e2: P[E, Int])(implicit L: Lambda[P]) =
    L.add(e1, e2)

  def lam[P[_, _], E, T1, T2](b: P[(T1, E), T2])(implicit L: Lambda[P]) =
    L.lam(b)

  def vz[P[_, _], E, T](implicit L: Lambda[P]): P[(T, E), T] =
    L.vz

  def vs[P[_, _], E, T, T1](a: P[E, T])(implicit L: Lambda[P]): P[(T1, E), T] =
    L.vs(a)

  implicit class AppOp[P[_, _], E, T1, T2](f: P[E, T1 => T2])(
      implicit
      L: Lambda[P]
  ) {
    def apply(a: P[E, T1]): P[E, T2] =
      L.app(f)(a)
  }
}
