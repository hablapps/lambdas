package lambdas
package taglessfinal
package hoas
package semantics

trait Term[T]{
  def apply[P[_]](implicit L: Lambda[P]): P[T]
}

object Term{

  implicit object TermLambda extends Lambda[Term]{

    def int(i: Int): Term[Int] = new Term[Int]{
      def apply[P[_]](implicit L: Lambda[P]) =
        L.int(i)
    }

    def add(i1: Term[Int])(i2: Term[Int]): Term[Int] = new Term[Int]{
      def apply[P[_]](implicit L: Lambda[P]) =
        L.add(i1.apply[P])(i2.apply[P])
    }

    def lam[T1, T2](f: Term[T1] => Term[T2]): Term[T1 => T2] = new Term[T1 => T2]{
      def apply[P[_]](implicit L: Lambda[P]): P[T1 => T2] =
        ??? // L.lam{ pt1: P[T1] => f(pt1).apply[P] : P[T2] }
    }

    def app[T1, T2](f: Term[T1 => T2])(t1: Term[T1]): Term[T2] = new Term[T2]{
      def apply[P[_]](implicit L: Lambda[P]) =
        L.app(f.apply[P])(t1.apply[P])
    }
  }
}
