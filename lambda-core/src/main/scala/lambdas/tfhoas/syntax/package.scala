package lambdas
package tfhoas

package object syntax {

  implicit class Lam1Ops[P[_], A, B](c: P[A] => P[B])(implicit L: Lambda[P]) {
    def lambda: P[A => B] = L.lam(c)
  }

  implicit class Lam2Ops[P[_], A, B, C](c: (P[A], P[B]) => P[C])(implicit L: Lambda[P]) {
    def lambda: P[(A, B) => C] = L.lam2(c)
  }

  implicit class AppOps[P[_], A](c: P[A])(implicit L: Lambda[P]) {
    def |>[B](f: P[A => B]): P[B] = L.app(f)(c)
  }

  implicit class TupleOps[P[_], A, B](c: (P[A], P[B]))(implicit L: Lambda[P]) {
    def tuple: P[(A, B)] = L.tuple(c._1, c._2)
  }

  implicit class ExtractorOps[P[_], A, B](c: P[(A, B)])(implicit L: Lambda[P]) {
    def fst: P[A] = L.fst(c)
    def snd: P[B] = L.snd(c)
  }

  implicit class Function2Ops[P[_], A, B, C](c: P[(A, B) => C])(implicit L: Lambda[P]) {
    def tupled: P[((A, B)) => C]      = L.tupled(c)
    def curried: P[A => B => C]       = L.curried(c)
    def apply(a: P[A], b: P[B]): P[C] = b |> { a |> c.curried }
  }

  implicit class Function2CurriedOps[P[_], A, B, C](c: P[A => B => C])(implicit L: Lambda[P]) {
    def apply(a: P[A], b: P[B]): P[C] = b |> { a |> c }
  }
}
