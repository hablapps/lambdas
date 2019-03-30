package lambdas
package tfhoas

import ArrowType.Implicits._

package object syntax {

  implicit class Lam1Ops[Type[_]: ArrowType, P[_], A: Type, B: Type](c: P[A] => P[B])(
      implicit L: Lambda[Type, P]
  ) {
    def lambda: P[A => B] = L.lam(c)
  }

  implicit class Lam2Ops[Type[_]: ArrowType, P[_], A: Type, B: Type, C: Type](
      c: (P[A], P[B]) => P[C]
  )(
      implicit L: Lambda[Type, P]
  ) {
    def lambda: P[(A, B) => C] = L.lam2(c)
  }

  implicit class AppOps[Type[_]: ArrowType, P[_], A: Type](c: P[A])(implicit L: Lambda[Type, P]) {
    def |>[B: Type](f: P[A => B]): P[B] = L.app(f)(c)
  }

  implicit class TupleOps[Type[_]: ArrowType, P[_], A: Type, B: Type](c: (P[A], P[B]))(
      implicit L: Lambda[Type, P]
  ) {
    def tuple: P[(A, B)] = L.tuple(c._1, c._2)
  }

  implicit class ExtractorOps[Type[_]: ArrowType, P[_], A: Type, B: Type](c: P[(A, B)])(
      implicit L: Lambda[Type, P]
  ) {
    def fst: P[A] = L.fst(c)
    def snd: P[B] = L.snd(c)
  }

  implicit class Function2Ops[Type[_], P[_], A, B, C](
      c: P[(A, B) => C]
  )(
      implicit L: Lambda[Type, P],
      AT: ArrowType[Type],
      TA: Type[A],
      TB: Type[B],
      TC: Type[C]
  ) {
    def tupled: P[((A, B)) => C]      = L.tupled(c)
    def curried: P[A => B => C]       = L.curried(c)
    def apply(a: P[A], b: P[B]): P[C] = b |> { a |> c.curried }
  }

  implicit class Function2CurriedOps[Type[_]: ArrowType, P[_], A: Type, B: Type, C: Type](
      c: P[A => B => C]
  )(
      implicit L: Lambda[Type, P]
  ) {
    def apply(a: P[A], b: P[B]): P[C] = b |> { a |> c }
  }
}
