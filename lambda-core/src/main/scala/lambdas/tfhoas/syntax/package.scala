package lambdas
package tfhoas

import ArrowType.Implicits._

package object syntax {

  implicit class Lam1Ops[P[_], A, B](c: P[A] => P[B]) {
    def lambda[Type[_]: ArrowType](implicit L: Lambda[Type, P], A: Type[A], B: Type[B]): P[A => B] =
      L.lam(c)
  }

  implicit class AppOps[Type[_]: ArrowType, P[_], A: Type](c: P[A])(implicit L: Lambda[Type, P]) {
    def |>[B: Type](f: P[A => B]): P[B] = L.app(f)(c)
  }

  implicit class TupleOps[P[_], A, B](c: (P[A], P[B]))(
      implicit P: Products[P]
  ) {
    def tuple: P[(A, B)] = P.tuple(c._1, c._2)
  }

  implicit class Lam2Ops[P[_], A, B, C](f: (P[A], P[B]) => P[C]) {
    def lambda[Type[_]: ArrowType](
        implicit L: Lambda[Type, P],
        TA: Type[A],
        TB: Type[B],
        TC: Type[C]
    ): P[A => B => C] =
      L.lam { pa: P[A] =>
        L.lam { pb: P[B] =>
          f(pa, pb)
        }
      }
  }

  implicit class ExtractorOps[P[_], A, B](c: P[(A, B)])(
      implicit P: Products[P]
  ) {
    def fst: P[A] = P.fst(c)
    def snd: P[B] = P.snd(c)
  }

  implicit class Function2CurriedOps[P[_], A, B, C](c: P[A => B => C]) {
    def apply[Type[_]: ArrowType](a: P[A], b: P[B])(
        implicit
        L: Lambda[Type, P],
        TA: Type[A],
        TB: Type[B],
        TC: Type[C]
    ): P[C] =
      b |> { a |> c }
  }
}
