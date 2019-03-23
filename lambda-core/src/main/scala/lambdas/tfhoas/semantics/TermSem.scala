package lambdas
package tfhoas
package semantics

trait Term[T] {
  def apply[P[_]](implicit L: Lambda[P]): P[T]
}

object Term {

  implicit object TermLambda extends Lambda[Term] {

    def tuple[A, B](a: Term[A], b: Term[B]): Term[(A, B)] = new Term[(A, B)] {
      def apply[P[_]](implicit L: Lambda[P]) = ???
    }

    def fst[A, B](t: Term[(A, B)]): Term[A] = new Term[A] {
      def apply[P[_]](implicit L: Lambda[P]) = ???
    }

    def snd[A, B](t: Term[(A, B)]): Term[B] = new Term[B] {
      def apply[P[_]](implicit L: Lambda[P]) = ???
    }

    def tupled[A, B, C](f: Term[(A, B) => C]): Term[((A, B)) => C] = new Term[((A, B)) => C] {
      def apply[P[_]](implicit L: Lambda[P]) = ???
    }

    def curried[A, B, C](f: Term[(A, B) => C]): Term[A => B => C] = new Term[A => B => C] {
      def apply[P[_]](implicit L: Lambda[P]) = ???
    }

    def lam[T1, T2](f: Term[T1] => Term[T2]): Term[T1 => T2] = new Term[T1 => T2] {
      def apply[P[_]](implicit L: Lambda[P]): P[T1 => T2] =
        ??? // L.lam{ pt1: P[T1] => f(...pt1).apply[P] : P[T2] }
    }

    def lam2[A, B, C](f: (Term[A], Term[B]) => Term[C]): Term[(A, B) => C] = new Term[(A, B) => C] {
      def apply[P[_]](implicit L: Lambda[P]) = ???
    }

    def app[T1, T2](f: Term[T1 => T2])(t1: Term[T1]): Term[T2] = new Term[T2] {
      def apply[P[_]](implicit L: Lambda[P]) =
        L.app(f.apply[P])(t1.apply[P])
    }
  }
}
