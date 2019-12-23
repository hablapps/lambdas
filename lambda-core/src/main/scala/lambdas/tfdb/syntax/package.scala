package lambdas
package tfdb

package object syntax {

  def lam[Type[_]: ArrowType, P[_, _], E, T1: Type, T2: Type](
      b: P[(T1, E), T2]
  )(implicit L: Lambda[Type, P]) =
    L.lam(b)

  def vz[Type[_]: ArrowType, P[_, _], E, T: Type](implicit L: Lambda[Type, P]): P[(T, E), T] =
    L.vz

  def vs[Type[_]: ArrowType, P[_, _], E, T: Type, T1: Type](
      a: P[E, T]
  )(implicit L: Lambda[Type, P]): P[(T1, E), T] =
    L.vs(a)

  implicit class AppOp[Type[_]: ArrowType, P[_, _], E, T1: Type, T2: Type](f: P[E, T1 => T2])(
      implicit
      L: Lambda[Type, P]
  ) {
    def apply(a: P[E, T1]): P[E, T2] =
      L.app(f)(a)
  }
}
