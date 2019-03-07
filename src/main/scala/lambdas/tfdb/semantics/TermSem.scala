package lambdas
package tfdb
package semantics

trait Term[E, T] {
  def apply[P[_, _]](implicit L: Lambda[P]): P[E, T]
}

object Term {

  object TermLambda extends Lambda[Term] {

    def int[E](i: Int) = new Term[E, Int] {
      def apply[P[_, _]](implicit L: Lambda[P]): P[E, Int] =
        L.int(i)
    }

    def add[E](i1: Term[E, Int], i2: Term[E, Int]) = new Term[E, Int] {
      def apply[P[_, _]](implicit L: Lambda[P]): P[E, Int] =
        L.add(i1(L), i2(L))
    }

    def vz[E, T] = new Term[(T, E), T] {
      def apply[P[_, _]](implicit L: Lambda[P]): P[(T, E), T] =
        L.vz
    }

    def vs[E, T, T1](a: Term[E, T]) = new Term[(T1, E), T] {
      def apply[P[_, _]](implicit L: Lambda[P]): P[(T1, E), T] =
        L.vs(a(L))
    }

    def lam[E, T1, T2](t: Term[(T1, E), T2]) = new Term[E, T1 => T2] {
      def apply[P[_, _]](implicit L: Lambda[P]): P[E, T1 => T2] =
        L.lam(t(L))
    }

    def app[E, T1, T2](f: Term[E, T1 => T2])(t1: Term[E, T1]) = new Term[E, T2] {
      def apply[P[_, _]](implicit L: Lambda[P]): P[E, T2] =
        L.app(f(L))(t1(L))
    }
  }
}
