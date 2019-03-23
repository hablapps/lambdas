package lambdas
package tfdb

package object syntax {

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
