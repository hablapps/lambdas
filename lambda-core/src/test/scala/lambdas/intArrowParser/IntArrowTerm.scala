package lambdas
package intArrowParser

import safecast._
import trees._, syntax._
import arithmetic.{ Arithmetic, IntType }
import tfdb.{ ArrowType, Lambda }, tfdb.semantics._

trait IntArrowTerm[E, T] {
  def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T]
}

object IntArrowTerm {

  implicit object IntArrowTermArith extends ForAll[IntArrowTerm, Arithmetic] {

    def apply[E] = new Arithmetic[IntArrowTerm[E, ?]] {
      def int(i: Int) = new IntArrowTerm[E, Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
          A[E].int(i)
      }

      def add(i1: IntArrowTerm[E, Int])(i2: IntArrowTerm[E, Int]) =
        new IntArrowTerm[E, Int] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
            A[E].add(i1(L, A))(i2(L, A))
        }

      def * = new IntArrowTerm[E, (Int, Int) => Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
          A[E].*
      }

      def + = new IntArrowTerm[E, (Int, Int) => Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
          A[E].+
      }
    }
  }

  implicit object IntArrowTermLambda extends Lambda[IntArrowTerm] {

    def int[E](i: Int) = new IntArrowTerm[E, Int] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, Int] =
        A[E].int(i)
    }

    def add[E](i1: IntArrowTerm[E, Int], i2: IntArrowTerm[E, Int]) =
      new IntArrowTerm[E, Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, Int] =
          A[E].add(i1(L, A))(i2(L, A))
      }

    def vz[E, T] = new IntArrowTerm[(T, E), T] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[(T, E), T] =
        L.vz
    }

    def vs[E, T, T1](a: IntArrowTerm[E, T]) = new IntArrowTerm[(T1, E), T] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[(T1, E), T] =
        L.vs(a(L, A))
    }

    def lam[E, T1, T2](t: IntArrowTerm[(T1, E), T2]) = new IntArrowTerm[E, T1 => T2] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T1 => T2] =
        L.lam(t(L, A))
    }

    def app[E, T1, T2](f: IntArrowTerm[E, T1 => T2])(t1: IntArrowTerm[E, T1]) =
      new IntArrowTerm[E, T2] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T2] =
          L.app(f(L, A))(t1(L, A))
      }
  }
}
