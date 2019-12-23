package lambdas
package tfdb
package semantics

abstract class Term[Type[_]: ArrowType, E, T] {
  def apply[P[_, _]](implicit L: Lambda[Type, P]): P[E, T]
}

object Term {

  class TermLambda[Type[_]: ArrowType] extends Lambda[Type, Term[Type, ?, ?]] {

    def vz[E, T: Type] = new Term[Type, (T, E), T] {
      def apply[P[_, _]](implicit L: Lambda[Type, P]): P[(T, E), T] =
        L.vz
    }

    def vs[E, T: Type, T1: Type](a: Term[Type, E, T]) = new Term[Type, (T1, E), T] {
      def apply[P[_, _]](implicit L: Lambda[Type, P]): P[(T1, E), T] =
        L.vs(a(L))
    }

    def lam[E, T1: Type, T2: Type](t: Term[Type, (T1, E), T2]) = new Term[Type, E, T1 => T2] {
      def apply[P[_, _]](implicit L: Lambda[Type, P]): P[E, T1 => T2] =
        L.lam(t(L))
    }

    def app[E, T1: Type, T2: Type](f: Term[Type, E, T1 => T2])(t1: Term[Type, E, T1]) =
      new Term[Type, E, T2] {
        def apply[P[_, _]](implicit L: Lambda[Type, P]): P[E, T2] =
          L.app(f(L))(t1(L))
      }
  }
}
