package lambdas
package taglessfinal
package debruijn

trait Lambda[P[E, T]]{
  def int[E](i: Int): P[E, Int]
  def add[E](i1: P[E, Int], i2: P[E, Int]): P[E, Int]
  def vz[E, T]: P[(T, E), T]
  def vs[E, T, T1](a: P[E, T]): P[(T1, E), T]
  def lam[E, T1, T2](t: P[(T1, E), T2]): P[E, T1 => T2]
  def app[E, T1, T2](f: P[E, T1 => T2])(t1: P[E, T1]): P[E, T2]
}

object Lambda{
  def apply[P[E, T]](implicit L: Lambda[P]) = L

  implicit val ShowSem: Lambda[ShowB] = semantics.ShowSem
  implicit val StdSem: Lambda[Function1] = semantics.Standard
  implicit val TermSem: Lambda[semantics.Term] = semantics.Term.TermLambda

  trait Syntax{

    implicit def int[P[_, _], E](i: Int)(implicit L: Lambda[P]): P[E, Int] =
      L.int[E](i)

    implicit class AddOp[P[_, _], E](e1: P[E, Int])(implicit L: Lambda[P]){
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

    implicit class AppOp[P[_, _], E, T1, T2](f: P[E, T1 => T2])(implicit
      L: Lambda[P]){
      def apply(a: P[E, T1]): P[E, T2] =
        L.app(f)(a)
    }
  }
}
